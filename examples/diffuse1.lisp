#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2016 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-examples.diffuse1
  (:use :cl
        :cffi
        :oclcl
        :eazy-opencl.host)
  (:export :main))
(in-package :oclcl-examples.diffuse1)

;;; image output functions

(declaim (inline index))
(defun index (nx jx jy)
  (the fixnum (+ (the fixnum (* nx jy)) jx)))

(defun image-value (f i j nx fmax fmin)
  (let ((fc (memory-block-aref f (index nx i j))))
    (truncate (* 256.0
                 (/ (- fc fmin) (- fmax fmin))))))

(defun file-name (dir i nout)
  (let ((n (truncate (/ i nout))))
    (concatenate 'string dir (format nil "~4,'0D.pgm" n))))

(defun output-pnm (dir i nout nx ny f)
  (let ((image (make-instance 'imago:grayscale-image
                              :width nx :height ny)))
    (dotimes (i nx)
      (dotimes (j ny)
        (setf (imago:image-pixel image i j) (image-value f i j nx 1.0 0.0))))
    (imago:write-pnm image (file-name dir i nout) :ASCII))
  (values))


;;; print functions

(defun print-elapsed-time (elapsed-time)
  (let ((time (* elapsed-time 1.0e-3)))
    (format t "Elapsed Time = ~,3F [sec]~%" time)))

(defun print-performance (flo elapsed-time)
  (let ((time (* elapsed-time 1.0e-3)))
    (format t "Performance = ~,2F [MFlops]~%" (* (/ flo time) 1.0e-6))))

(defun print-time (cnt time)
  (format t "time(~A) = ~,5F~%" cnt time))


;;; main functions

(defkernel diffusion2d (void ((f float*) (fn float*)
                              (nx int) (ny int)
                              (c0 float) (c1 float) (c2 float)))
  (let ((local-id-x (size-t-to-int (get-local-id 0)))
        (local-id-y (size-t-to-int (get-local-id 1)))
        (global-id-x (size-t-to-int (get-global-id 0)))
        (global-id-y (size-t-to-int (get-global-id 1)))
        (local-size-x (size-t-to-int (get-local-size 0)))
        (local-size-y (size-t-to-int (get-local-size 1)))
        (group-id-x (size-t-to-int (get-group-id 0)))
        (group-id-y (size-t-to-int (get-group-id 1)))
        (num-groups-x (size-t-to-int (get-num-groups 0)))
        (num-groups-y (size-t-to-int (get-num-groups 1))))
    (let* ((jx (+ local-id-x 1))
           (jy (+ local-id-y 1))
           (j (+ (* nx global-id-y) global-id-x))
           (fcc (aref f j)))
      (with-shared-memory ((fs float (+ 16 2) (+ 16 2)))
        (set (aref fs jy jx) fcc)
        (if (= local-id-x 0)
            (if (= group-id-x 0)
                (set (aref fs jy 0) fcc)
                (set (aref fs jy 0) (aref f (- j 1)))))
        (if (= local-id-x (- local-size-x 1))
            (if (= group-id-x (- num-groups-x 1))
                (set (aref fs jy (+ local-size-x 1)) fcc)
                (set (aref fs jy (+ local-size-x 1)) (aref f (+ j 1)))))
        (if (= local-id-y 0)
            (if (= group-id-y 0)
                (set (aref fs 0 jx) fcc)
                (set (aref fs 0 jx) (aref f (- j nx)))))
        (if (= local-id-y (- local-size-y 1))
            (if (= group-id-y (- num-groups-y 1))
                (set (aref fs (+ local-size-y 1) jx) fcc)
                (set (aref fs (+ local-size-y 1) jx) (aref f (+ j nx)))))
        (barrier :clk-local-mem-fence)
        (set (aref fn j) (+ (* c0 (+ (aref fs jy (+ jx 1))
                                     (aref fs jy (- jx 1))))
                            (* c1 (+ (aref fs (+ jy 1) jx)
                                     (aref fs (- jy 1) jx)))
                            (* c2 (aref fs jy jx))))))))

(defun initialize-device-memory (nx ny dx dy command-queue host-memory device-memory)
  (let ((alpha 30.0))
    (dotimes (jy ny)
      (dotimes (jx nx)
        (let ((j (index nx jx jy))
              (x (- (* dx (+ (float jx 1.0) 0.5)) 0.5))
              (y (- (* dy (+ (float jy 1.0) 0.5)) 0.5)))
          (setf (mem-aref host-memory :float j)
                (exp (* (- alpha)
                        (+ (* x x) (* y y)))))))))
  (%ocl:enqueue-write-buffer command-queue
                             device-memory
                             %ocl:true
                             0
                             (* (foreign-type-size :float)
                                (* nx ny))
                             host-memory
                             0
                             (null-pointer)
                             (null-pointer)))

(defun diffusion2d (nx ny command-queue kernel f fn kappa dt dx dy)
  (let* ((c0 (* kappa (/ dt (* dx dx))))
         (c1 (* kappa (/ dt (* dy dy))))
         (c2 (- 1.0 (* 2.0 (+ c0 c1)))))
    (%ocl/h::with-foreign-array (global-work-size '%ocl:size-t (list nx ny))
      (set-kernel-arg kernel 0 f '%ocl:mem)
      (set-kernel-arg kernel 1 fn '%ocl:mem)
      (set-kernel-arg kernel 2 nx :int)
      (set-kernel-arg kernel 3 ny :int)
      (set-kernel-arg kernel 4 c0 :float)
      (set-kernel-arg kernel 5 c1 :float)
      (set-kernel-arg kernel 6 c2 :float)
      (%ocl:enqueue-nd-range-kernel command-queue
                                    kernel
                                    2
                                    (null-pointer)
                                    global-work-size
                                    (null-pointer)
                                    0
                                    (null-pointer)
                                    (null-pointer)))
    (%ocl:finish command-queue)
    (* nx ny 7.0)))

(defmacro swap (a b)
  `(rotatef ,a ,b))

(defun main ()
  (let* ((nx 256) (ny 256)
         (Lx 1.0) (Ly 1.0)
         (dx (/ Lx (float nx 1.0)))
         (dy (/ Ly (float ny 1.0)))
         (kappa 0.1)
         (dt (/ (* 0.2 (min (* dx dx) (* dy dy))) kappa))
         (time 0)
         (flo 0)
         (platform-id (car (get-platform-ids)))
         (devices (get-device-ids platform-id :device-type-default))
         (context (create-context devices))
         (command-queue (create-command-queue context (car devices) 0))
         (c-source-code (kernel-manager-translate *kernel-manager*))
         (program (create-program-with-source context c-source-code))
         (elements (* nx ny))
         (float-size (foreign-type-size :float))
         (data-bytes (* float-size elements)))
    (with-foreign-objects ((a-host :float elements))
      (let* ((a-device (create-buffer context :mem-read-only data-bytes))
             (b-device (create-buffer context :mem-read-only data-bytes)))
        (initialize-device-memory nx ny dx dy command-queue a-host a-device)
        (build-program program :devices devices)
        (let ((kernel (create-kernel program "oclcl_examples_diffuse1_diffusion2d")))
          (dotimes (i 20000)
            (when (= (mod i 100) 0)
              (print-time i time))
            (incf flo (diffusion2d nx ny command-queue kernel a-device b-device kappa dt dx dy))
            (incf time dt)
            (swap a-device b-device)))))))
