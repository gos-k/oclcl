#|
  This file is a part of oclcl project.
  Copyright (c) 2013 Masayuki Takagi (kamonama@gmail.com)
                2016 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-examples.sph-oclapi
  (:use :cl
        :cffi
        :oclcl
        :cl-oclapi)
  (:import-from :alexandria
                :with-gensyms
                :once-only)
  (:export :main))
(in-package :oclcl-examples.sph-oclapi)

(define-program :sph-oclapi
  (:use :oclcl))
(in-program :sph-oclapi)

;;
;; Utilities

(defkernel norm (float ((x float4)))
  (return (sqrt (+ (* (float4-x x) (float4-x x))
                   (* (float4-y x) (float4-y x))
                   (* (float4-z x) (float4-z x))
                   (* (float4-w x) (float4-w x))))))

(defkernelmacro do-range ((var from to) &body body)
  `(do ((,var ,from (+ ,var 1)))
       ((> ,var ,to))
     ,@body))

(defkernelmacro and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args) (and ,@(cdr args)) nil))))

(defkernelmacro inc (place val)
  `(set ,place (+ ,place ,val)))

(defkernelmacro pow (x n)
  (check-type n fixnum)
  `(* ,@(loop repeat n collect x)))

;; (defkernel pow (float ((b float) (p float)))
;;   (return (expt b p)))

(defkernelmacro with-particle-index ((var) &body body)
  `(let ((,var (to-int (get-global-id 0))))
     ,@body))


;;
;; Parameters

(defkernel-symbol-macro h 0.005)
(defkernel-symbol-macro dt 0.0004)
(defkernel-symbol-macro pi 3.1415927)
(defkernel-symbol-macro visc 0.2)
(defkernel-symbol-macro limit 200.0)
(defkernel-symbol-macro pmass (/ 0.00020543 8.0))
(defkernel-symbol-macro radius 0.002)
(defkernel-symbol-macro epsilon 0.00001)
(defkernel-symbol-macro extdamp 512.0)
(defkernel-symbol-macro simscale 0.004)
(defkernel-symbol-macro intstiff 3.0)
(defkernel-symbol-macro extstiff 20000.0)
(defkernel-symbol-macro restdensity 600.0)
(defkernel-symbol-macro g (float4 0.0 -9.8 0.0 0.0))

(defmemory box-min (float4 -10.0  0.0 -10.0 0.0) :constant)
(defmemory box-max (float4 30.0 50.0  30.0 0.0) :constant)
(defmemory origin (float4 (- -10.0 (* (/ 0.005 0.004) 2.0))
                          (-  0.0 (* (/ 0.005 0.004) 2.0))
                          (- -10.0 (* (/ 0.005 0.004) 2.0))
                          0.0) :constant)
(defmemory delta (/ 0.005 0.004) :constant)
(defmemory capacity 400 :constant)
(defmemory size-x 37 :constant)
(defmemory size-y 45 :constant)
(defmemory size-z 37 :constant)

(defparameter h           0.005)
(defparameter pmass       (/ 0.00020543 8.0))
(defparameter simscale    0.004)
(defparameter restdensity 600.0)
(defparameter pdist       (expt (/ pmass restdensity) (/ 1.0 3.0)))
(defparameter g           '(0.0 -9.8 0.0 0.0))
(defparameter delta       (/ h simscale))
(defparameter box-min     '(-10.0  0.0 -10.0 0.0))
(defparameter box-max     '(30.0 50.0  30.0 0.0))
(defparameter init-min    '(-10.0  0.0 -10.0 0.0))
(defparameter init-max    '(0.0 40.0  30.0 0.0))
(defparameter capacity    400)  ; # of particles contained in one cell

;;
;; Neighbor map

(defkernelmacro with-cell-index (((i j k) x) &body body)
  (once-only (x)
    `(let ((,i (to-int (floor (/ (- (float4-x ,x) (float4-x origin)) delta))))
           (,j (to-int (floor (/ (- (float4-y ,x) (float4-y origin)) delta))))
           (,k (to-int (floor (/ (- (float4-z ,x) (float4-z origin)) delta)))))
       ,@body)))

(defkernel count-offset (int ((i int) (j int) (k int)))
  (return (+ (* size-x size-y k)
             (* size-x j)
             i)))

(defkernel map-offset (int ((i int) (j int) (k int) (l int)))
  (return (+ (* capacity size-x size-y k)
             (* capacity size-x j)
             (* capacity i)
             l)))

(defkernel update-neighbor-map (void ((neighbor-count int*)
                                      (neighbor-map int*)
                                      (pos float4*)))
  (with-particle-index (p)
    (with-cell-index ((i j k) (aref pos p))
      (let ((co (count-offset i j k)))
        ;; Atomically increment the number of particles in the cell.
        (let ((l (atomic-inc (pointer (aref neighbor-count co)))))
          ;; Set particle in the cell.
          (set (aref neighbor-map (map-offset i j k l)) p))))))

(defkernel clear-neighbor-count (void ((neighbor-count int*)))
  (set (aref neighbor-count (count-offset (to-int (get-global-id 0))
                                          (to-int (get-global-id 1))
                                          (to-int (get-global-id 2))))
       0))

(defkernelmacro do-neighbors ((var neighbor-count neighbor-map x) &body body)
  (with-gensyms (i0 j0 k0 i j k l size)
    `(with-cell-index ((,i0 ,j0 ,k0) ,x)
      (declare "pragma" "unroll")
       (do-range (,i (- ,i0 1) (+ ,i0 1))
         (declare "pragma" "unroll")
         (do-range (,j (- ,j0 1) (+ ,j0 1))
           (declare "pragma" "unroll")
           (do-range (,k (- ,k0 1) (+ ,k0 1))
             (let ((,size (aref ,neighbor-count (count-offset ,i ,j ,k))))
               (declare "pragma" "unroll")
               (do-range (,l 0 (- ,size 1))
                 (when (< ,l capacity)
                   (let ((,var (aref ,neighbor-map (map-offset ,i ,j ,k ,l))))
                     ,@body))))))))))

(defun compute-origin (box-min delta)
  (let ((delta2 (* delta 2)))
    (list (- (nth 0 box-min) delta2)
          (- (nth 1 box-min) delta2)
          (- (nth 2 box-min) delta2)
          0.0)))

(defun compute-size (box-min box-max delta capacity)
  (assert (and (< (nth 0 box-min) (nth 0 box-max))
               (< (nth 1 box-min) (nth 1 box-max))
               (< (nth 2 box-min) (nth 2 box-max))))
  (assert (< 0.0 delta))
  (assert (< 0 capacity))
  (flet ((compute-size1 (x0 x1)
           (+ (ceiling (/ (- x1 x0) delta))
              4)))
    (let* ((size-x (compute-size1 (nth 0 box-min) (nth 0 box-max)))
           (size-y (compute-size1 (nth 1 box-min) (nth 1 box-max)))
           (size-z (compute-size1 (nth 2 box-min) (nth 2 box-max)))
           (size (* size-x
                    size-y
                    size-z
                    capacity)))
      (values size-x size-y size-z size))))


;;
;; Boundary condition

;; returns dummy integer to avoid __host__ qualifier
(defkernel apply-collision (int ((acc float4*)
                                 (i int)
                                 (x0 float)
                                 (x1 float)
                                 (v float4)
                                 (normal float4)))
  (let* ((distance (* (- x1 x0) simscale))
         (diff (- (* radius 2.0) distance))
         (adj (- (* extstiff diff)
                 (* extdamp (dot normal v)))))
    (when (< epsilon diff)
      (inc (aref acc i) (* adj normal))))
  (return 0))

;; returns dummy integer to avoid __host__ qualifier
(defkernel apply-accel-limit (int ((acc float4*) (i int)))
  (let ((accel (norm (aref acc i))))
    (when (< limit accel)
      (set (aref acc i) (* (aref acc i) (/ limit accel)))))
  (return 0))

(defkernel boundary-condition (void ((acc float4*)
                                     (pos float4*)
                                     (vel float4*)))
  (with-particle-index (i)
    (let ((xi (aref pos i))
          (vi (aref vel i)))
      ;; Left boundary.
      (apply-collision acc i (float4-x box-min) (float4-x xi) vi
                       (float4 1.0 0.0 0.0 0.0))
      ;; Right boundary.
      (apply-collision acc i (float4-x xi) (float4-x box-max) vi
                       (float4 -1.0 0.0 0.0 0.0))
      ;; Bottom boundary.
      (apply-collision acc i (float4-y box-min) (float4-y xi) vi
                       (float4 0.0 1.0 0.0 0.0))
      ;; Top boundary.
      (apply-collision acc i (float4-y xi) (float4-y box-max) vi
                       (float4 0.0 -1.0 0.0 0.0))
      ;; Near side boundary.
      (apply-collision acc i (float4-z box-min) (float4-z xi) vi
                       (float4 0.0 0.0 1.0 0.0))
      ;; Far side boundary.
      (apply-collision acc i (float4-z xi) (float4-z box-max) vi
                       (float4 0.0 0.0 -1.0 0.0))
      ;; Accel limit.
      (apply-accel-limit acc i))))


;;
;; SPH kernel functions

(defkernel poly6-kernel (float ((x float4)))
  (let ((r (norm x)))
    (return (* (/ 315.0 (* 64.0 pi (pow h 9)))
               (pow (- (* h h) (* r r)) 3)))))

(defkernel grad-spiky-kernel (float4 ((x float4)))
  (let ((r (norm x)))
    (return (* (/ -45.0 (* pi (pow h 6)))
               (pow (- h r) 2)
               (/ x r)))))

(defkernel rap-visc-kernel (float ((x float4)))
  (let ((r (norm x)))
    (return (* (/ 45.0 (* pi (pow h 6)))
               (- h r)))))


;;
;; Update density

(defkernel update-density (void ((rho float*)
                                 (pos float4*)
                                 (neighbor-count int*)
                                 (neighbor-map int*)))
  (with-particle-index (i)
    (let ((xi (aref pos i))
          (tmp 0.0))
      (do-neighbors (j neighbor-count neighbor-map xi)
        (let* ((xj (aref pos j))
               (dr (* (- xi xj) simscale)))
          (when (<= (norm dr) h)
            (inc tmp (* pmass (poly6-kernel dr))))))
      (set (aref rho i) tmp))))


;;
;; Update pressure

(defkernel update-pressure (void ((prs float*)
                                  (rho float*)))
  (with-particle-index (i)
    (set (aref prs i) (* (- (aref rho i) restdensity)
                         intstiff))))


;;
;; Update force

(defkernel pressure-term (float4 ((rho float*)
                                  (prs float*)
                                  (i int)
                                  (j int)
                                  (dr float4)))
  (return (* (/ (* (- pmass) (+ (aref prs i) (aref prs j)))
                (* 2.0 (aref rho j)))
             (grad-spiky-kernel dr))))

(defkernel viscosity-term (float4 ((vel float4*)
                                   (rho float*)
                                   (i int)
                                   (j int)
                                   (dr float4)))
  (return (* (/ (* visc pmass (- (aref vel j) (aref vel i)))
                (aref rho j))
             (rap-visc-kernel dr))))

(defkernel update-force (void ((force float4*)
                               (pos float4*)
                               (vel float4*)
                               (rho float*)
                               (prs float*)
                               (neighbor-count int*)
                               (neighbor-map int*)))
  (with-particle-index (i)
    (let ((xi (aref pos i))
          (tmp (float4 0.0 0.0 0.0 0.0)))
      (do-neighbors (j neighbor-count neighbor-map xi)
        (when (/= i j)
          (let* ((xj (aref pos j))
                 (dr (* (- xi xj) simscale)))
            (when (<= (norm dr) h)
              (inc tmp (pressure-term rho prs i j dr))
              (inc tmp (viscosity-term vel rho i j dr))))))
      (set (aref force i) tmp))))


;;
;; Update acceleration

(defkernel update-acceleration (void ((acc float4*)
                                      (force float4*)
                                      (rho float*)))
  (with-particle-index (i)
    (set (aref acc i) (+ (/ (aref force i)
                            (aref rho i))
                         g))))


;;
;; Update velocity

(defkernel update-velocity (void ((vel float4*)
                                  (acc float4*)))

  (with-particle-index (i)
    (inc (aref vel i) (* (aref acc i) dt))))


;;
;; Update position

(defkernel update-position (void ((pos float4*)
                                  (vel float4*)))
  (with-particle-index (i)
    (inc (aref pos i) (/ (* (aref vel i) dt)
                         simscale))))


;;
;; Output functions

(defparameter +filename-template+ "result~8,'0d.pov")

(defparameter +header-template+ "#include \"colors.inc\"

camera {
  location <10, 30, -40>
  look_at <10, 10, 0>
}
light_source { <0, 30, -30> color White }

")

(defparameter +sphere-template+ "sphere {
  <~F,~F,~F>,0.25
  texture {
    pigment { color Yellow }
  }
}

")

(defun filename (i)
  (format nil +filename-template+ i))

(defun output-header (stream)
  (format stream +header-template+))

(defun output-sphere (x y z stream)
  (format stream +sphere-template+ x y z))

(defun output (command-queue step pos n)
  (format t "Output step ~A...~%" step)
  (let* ((float-size (foreign-type-size 'cl-float))
         (filename (filename step)))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (output-header out)
      (with-foreign-objects ((foreign-array 'cl-float (* 4 n)))
        (enqueue-read-buffer command-queue
                             pos
                             +cl-true+
                             0
                             (* 4 float-size n)
                             foreign-array)
        (loop for i from 0 below (* 4 n) by 4
              do (output-sphere (mem-aref foreign-array 'cl-float i)
                                (mem-aref foreign-array 'cl-float (+ 1 i))
                                (mem-aref foreign-array 'cl-float (+ 2 i))
                                out))))))


;;
;; Main

(defun initial-condition (init-min init-max d)
  (destructuring-bind (x0 y0 z0 w0) init-min
    (destructuring-bind (x1 y1 z1 w1) init-max
      (let (result)
        (loop for x from (+ x0 d) below x1 by d
           do (loop for y from (+ y0 d) below y1 by d
                 do (loop for z from (+ z0 d) below z1 by d
                       do (push (list x y z 0.0) result))))
        result))))

(defun set-float4 (forign-array index value0 value1 value2 value3)
  (labels ((set-element (offset value)
             (setf (mem-aref forign-array :float (+ (* 4 index) offset))
                   value)))
    (set-element 0 value0)
    (set-element 1 value1)
    (set-element 2 value2)
    (set-element 3 value3)))

(defun initialize (pos vel particles)
  (loop for p in particles
        for i from 0
        do (set-float4 pos i (nth 0 p) (nth 1 p) (nth 2 p) (nth 3 p))
           (set-float4 vel i 0.0 0.0 0.0 0.0)))

(defun main (&key pov-files interim-results)
  (with-platform-id (platform)
    (with-device-ids (devices num-devices platform)
      (with-context (context (null-pointer) 1 devices)
        (let ((*program* (find-program :sph-oclapi))
              (c-source-code (concatenate 'string
                                          "#pragma OPENCL EXTENSION cl_khr_global_int32_base_atomics : enable"
                                          (compile-program *program*)))
              (device (mem-aref devices 'cl-device-id)))
          ;;(pprint c-source-code)
          (with-program-with-source (program context 1 c-source-code)
            (build-program program 1 devices)
            (let* (;; Get initial condition.
                   (particles (initial-condition init-min init-max (/ pdist simscale)))
                   ;; Get number of particles.
                   (n (length particles))
                   ;; Compute neighbor map origin.
                   (origin (compute-origin box-min delta))
                   (int-size (foreign-type-size 'cl-int))
                   (float-size (foreign-type-size 'cl-float))
                   (float-n-size (* float-size n))
                   (float4-size (* float-size 4))
                   (float4-n-size (* float4-size n)))

              ;; Print number of particles.
              (format t "~A particles~%" n)
              (multiple-value-bind (size-x size-y size-z size)
                  (compute-size box-min box-max delta capacity)
                (with-foreign-objects ((pos 'cl-float (* 4 n))
                                       (vel 'cl-float (* 4 n)))
                  (initialize pos vel particles)
                  (when interim-results
                    (print-foreign-array pos (* 4 n) 'cl-float)
                    (print-foreign-array vel (* 4 n) 'cl-float))
                  (with-buffers ((pos-device context +cl-mem-read-write+ float4-n-size)
                                 (vel-device context +cl-mem-read-write+ float4-n-size)
                                 (acc-device context +cl-mem-read-write+ float4-n-size)
                                 (force-device context +cl-mem-read-write+ (* 4 float4-n-size))
                                 (rho-device context +cl-mem-read-write+ float-n-size)
                                 (prs-device context +cl-mem-read-write+ float-n-size)
                                 (neighbor-count-device context +cl-mem-read-write+ (* int-size size-x size-y size-z))
                                 (neighbor-map-device context +cl-mem-read-write+ (* int-size size)))
                    (with-pointers ((pos-pointer pos-device)
                                    (vel-pointer vel-device)
                                    (acc-pointer acc-device)
                                    (force-pointer force-device)
                                    (rho-pointer rho-device)
                                    (prs-pointer prs-device)
                                    (neighbor-count-pointer neighbor-count-device)
                                    (neighbor-map-pointer neighbor-map-device))
                      (with-command-queue (command-queue context device 0)
                        (labels ((write-buffer (device size host)
                                   (enqueue-write-buffer command-queue
                                                         device
                                                         +cl-true+
                                                         0
                                                         size
                                                         host)))
                          (write-buffer pos-device float4-n-size pos)
                          (write-buffer vel-device float4-n-size vel)
                          (finish command-queue))

                        ;; Grid and block dims.
                        (with-work-sizes ((neighbor-map-global-work-size 37 45 37)
                                          (neighbor-map-local-work-size 37 1 1)
                                          (particle-global-work-size n))
                          (labels ((c-name (name)
                                     (program-function-c-name *program* name))
                                   (print-interim-result (command-queue device size type &key (step 1) (index nil))
                                     (when interim-results
                                       (print-device-memory command-queue
                                                            device
                                                            size
                                                            type
                                                            :step step
                                                            :index index))))
                            ;; Do simulation time
                            (time
                             (loop repeat 300
                                   for i from 1
                                   do ;; Clear neighbor map.
                                      (with-kernel (kernel program (c-name 'clear-neighbor-count))
                                        (set-kernel-arg kernel 0 8 neighbor-count-pointer)
                                        (enqueue-ndrange-kernel command-queue
                                                                kernel
                                                                3
                                                                neighbor-map-global-work-size
                                                                neighbor-map-local-work-size)
                                        (finish command-queue))
                                      (print-interim-result command-queue neighbor-count-device (* size-x size-y size-z) 'cl-int)

                                      ;; Update neighbor map.
                                      (with-kernel (kernel program (c-name 'update-neighbor-map))
                                        (set-kernel-args kernel `((0 8 ,neighbor-count-pointer)
                                                                  (1 8 ,neighbor-map-pointer)
                                                                  (2 8 ,pos-pointer)))
                                        (enqueue-ndrange-kernel command-queue
                                                                kernel
                                                                1
                                                                particle-global-work-size)
                                        (finish command-queue))
                                      (print-interim-result command-queue neighbor-count-device (* size-x size-y size-z) 'cl-int)
                                      (print-interim-result command-queue neighbor-map-device size 'cl-int)

                                      ;; Update density.
                                      (with-kernel (kernel program (c-name 'update-density))
                                        (set-kernel-args kernel `((0 8 ,rho-pointer)
                                                                  (1 8 ,pos-pointer)
                                                                  (2 8 ,neighbor-count-pointer)
                                                                  (3 8 ,neighbor-map-pointer)))
                                        (enqueue-ndrange-kernel command-queue
                                                                kernel
                                                                1
                                                                particle-global-work-size)
                                        (finish command-queue))
                                      (print-interim-result command-queue rho-device n 'cl-float)

                                      ;; Update pressure.
                                      (with-kernel (kernel program (c-name 'update-pressure))
                                        (set-kernel-args kernel `((0 8 ,prs-pointer)
                                                                  (1 8 ,rho-pointer)))
                                        (enqueue-ndrange-kernel command-queue
                                                                kernel
                                                                1
                                                                particle-global-work-size)
                                        (finish command-queue))
                                      (print-interim-result command-queue prs-device n 'cl-float)

                                      ;; Update force.
                                      (with-kernel (kernel program (c-name 'update-force))
                                        (set-kernel-args kernel `((0 8 ,force-pointer)
                                                                  (1 8 ,pos-pointer)
                                                                  (2 8 ,vel-pointer)
                                                                  (3 8 ,rho-pointer)
                                                                  (4 8 ,prs-pointer)
                                                                  (5 8 ,neighbor-count-pointer)
                                                                  (6 8 ,neighbor-map-pointer)))
                                        (enqueue-ndrange-kernel command-queue
                                                                kernel
                                                                1
                                                                particle-global-work-size)
                                        (finish command-queue))
                                      (print-interim-result command-queue force-device (* 4 n) 'cl-float)

                                      ;; Update acceleration.
                                      (with-kernel (kernel program (c-name 'update-acceleration))
                                        (set-kernel-args kernel `((0 8 ,acc-pointer)
                                                                  (1 8 ,force-pointer)
                                                                  (2 8 ,rho-pointer)))
                                        (enqueue-ndrange-kernel command-queue
                                                                kernel
                                                                1
                                                                particle-global-work-size)
                                        (finish command-queue))
                                      (print-interim-result command-queue acc-device (* 4 n) 'cl-float)

                                      ;; Apply boundary condition.
                                      (with-kernel (kernel program (c-name 'boundary-condition))
                                        (set-kernel-args kernel `((0 8 ,acc-pointer)
                                                                  (1 8 ,pos-pointer)
                                                                  (2 8 ,vel-pointer)))
                                        (enqueue-ndrange-kernel command-queue
                                                                kernel
                                                                1
                                                                particle-global-work-size)
                                        (finish command-queue))
                                      (print-interim-result command-queue acc-device (* 4 n) 'cl-float)

                                      ;; Update velocity.
                                      (with-kernel (kernel program (c-name 'update-velocity))
                                        (set-kernel-args kernel `((0 8 ,vel-pointer)
                                                                  (1 8 ,acc-pointer)))
                                        (enqueue-ndrange-kernel command-queue
                                                                kernel
                                                                1
                                                                particle-global-work-size)
                                        (finish command-queue))
                                      (print-interim-result command-queue vel-device (* 4 n) 'cl-float)

                                      ;; Update position.
                                      (with-kernel (kernel program (c-name 'update-position))
                                        (set-kernel-args kernel `((0 8 ,pos-pointer)
                                                                  (1 8 ,vel-pointer)))
                                        (enqueue-ndrange-kernel command-queue
                                                                kernel
                                                                1
                                                                particle-global-work-size)
                                        (finish command-queue))
                                      (print-interim-result command-queue pos-device (* 4 n) 'cl-float)

                                      ;; Output POV file.
                                      (when pov-files
                                        (when (= (mod i 10) 0)
                                          (output command-queue i pos-device n)))))))))))))))))))
