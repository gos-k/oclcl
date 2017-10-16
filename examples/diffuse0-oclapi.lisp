#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2016 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-examples.diffuse0-oclapi
  (:use :cl
        :cffi
        :oclcl
        :cl-oclapi)
  (:export :main))
(in-package :oclcl-examples.diffuse0-oclapi)

(define-program :diffuse0-oclapi
  (:use :oclcl))
(in-program :diffuse0-oclapi)

;;; image output functions

(declaim (inline index))
(defun index (nx jx jy)
  (the fixnum (+ (the fixnum (* nx jy)) jx)))

(defun image-value (f i j nx fmax fmin)
  (let ((fc (mem-aref f :float (index nx i j))))
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
  (let* ((jy (to-int (get-global-id 1)))
         (jx (to-int (get-global-id 0)))
         (j (+ (* nx jy) jx)))
    (let ((fcc (aref f j))
          (fcw 0.0)
          (fce 0.0)
          (fcs 0.0)
          (fcn 0.0))
      (if (= jx 0)
          (set fcw fcc)
          (set fcw (aref f (- j 1))))
      (if (= jx (- nx 1))
          (set fce fcc)
          (set fce (aref f (+ j 1))))
      (if (= jy 0)
          (set fcs fcc)
          (set fcs (aref f (- j nx))))
      (if (= jy (- ny 1))
          (set fcn fcc)
          (set fcn (aref f (+ j nx))))
      (set (aref fn j) (+ (* c0 (+ fce fcw))
                          (* c1 (+ fcn fcs))
                          (* c2 fcc))))))

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

  (enqueue-write-buffer command-queue
                        device-memory
                        +cl-true+
                        0
                        (* (foreign-type-size :float)
                           (* nx ny))
                        host-memory)
  (finish command-queue))

(defun diffusion2d (nx ny command-queue kernel f fn kappa dt dx dy)
  (let* ((c0 (* kappa (/ dt (* dx dx))))
         (c1 (* kappa (/ dt (* dy dy))))
         (c2 (- 1.0 (* 2.0 (+ c0 c1)))))
    (with-work-size (global-work-size nx ny)
      (with-pointers ((f-pointer f)
                      (fn-pointer fn))
        (with-foreign-objects ((%nx 'cl-int)
                               (%ny 'cl-int)
                               (%c0 'cl-float)
                               (%c1 'cl-float)
                               (%c2 'cl-float))
          (setf (mem-aref %nx 'cl-int) nx)
          (setf (mem-aref %ny 'cl-int) ny)
          (setf (mem-aref %c0 'cl-float) c0)
          (setf (mem-aref %c1 'cl-float) c1)
          (setf (mem-aref %c2 'cl-float) c2)
          (set-kernel-arg kernel 0 8 f-pointer)
          (set-kernel-arg kernel 1 8 fn-pointer)
          (set-kernel-arg kernel 2 4 %nx)
          (set-kernel-arg kernel 3 4 %ny)
          (set-kernel-arg kernel 4 4 %c0)
          (set-kernel-arg kernel 5 4 %c1)
          (set-kernel-arg kernel 6 4 %c2)
          (enqueue-ndrange-kernel command-queue
                                  kernel
                                  2
                                  global-work-size
                                  (null-pointer))
          (finish command-queue))))
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
         (elements (* nx ny))
         (float-size (foreign-type-size :float))
         (data-bytes (* float-size elements))
         (*program* (find-program :diffuse0-oclapi))
         (c-source-code (compile-program *program*)))
    (print c-source-code)
    (with-platform-id (platform)
      (with-device-ids (devices num-devices platform)
        (with-context (context (null-pointer) 1 devices)
          (with-program-with-source (program context 1 c-source-code)
            (build-program program 1 devices)
            (with-foreign-objects ((a-host :float elements)
                                   (b-host :float elements))
              (with-buffers ((a-device context +cl-mem-read-only+ data-bytes)
                             (b-device context +cl-mem-write-only+ data-bytes))
                (let ((device (mem-aref devices 'cl-device-id)))
                  (with-command-queue (command-queue context device 0)
                    (initialize-device-memory nx ny dx dy command-queue a-host a-device)
                    (with-work-size (global-work-size elements)
                      (with-kernel (kernel program (program-function-c-name *program* 'diffusion2d))
                        (dotimes (i 20000)
                          (when (= (mod i 100) 0)
                            (print-time i time))
                          (incf flo (diffusion2d nx ny command-queue kernel a-device b-device kappa dt dx dy))
                          (incf time dt)
                          (swap a-device b-device))))))))))))))
