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

(defmemory box-min (float4 0.0 0.0 0.0 0.0) :constant)
(defmemory box-max (float4 0.0 0.0 0.0 0.0) :constant)
(defmemory origin (float4 0.0 0.0 0.0 0.0) :constant)
(defmemory delta 0.0 :constant)
(defmemory capacity 0 :constant)
(defmemory size-x 0 :constant)
(defmemory size-y 0 :constant)
(defmemory size-z 0 :constant)

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

#+nil
(defkernel set-params (void ((box-min-0 float)
                             (box-min-1 float)
                             (box-min-2 float)
                             (box-min-3 float)
                             (box-max-0 float)
                             (box-max-1 float)
                             (box-max-2 float)
                             (box-max-3 float)
                             (origin-0 float)
                             (origin-1 float)
                             (origin-2 float)
                             (origin-3 float)
                             (d float)
                             (c int)
                             (sx int)
                             (sy int)
                             (sz int)))

  (set box-min (float4 box-min-0
                       box-min-1
                       box-min-2
                       box-min-3))

  (set box-max (float4 box-max-0
                       box-max-1
                       box-max-2
                       box-max-3))
  (set origin (float4 origin-0
                      origin-1
                      origin-2
                      origin-3))
  (set delta d)
  (set capacity c)
  (set size-x sx)
  (set size-y sy)
  (set size-z sz))

;;
;; Neighbor map

(defkernelmacro with-cell-index (((i j k) x) &body body)
  (once-only (x)
    `(let ((,i (to-int (floor (/ (- (float4-x ,x) (float4-x origin)) delta))))
           (,j (to-int (floor (/ (- (float4-y ,x) (float4-y origin)) delta))))
           (,k (to-int (floor (/ (- (float4-z ,x) (float4-z origin)) delta)))))
       ,@body)))

(defkernel offset (int ((i int) (j int) (k int) (l int)))
  (return (+ (* capacity size-x size-y k)
             (* capacity size-x j)
             (* capacity i)
             l)))

(defkernel update-neighbor-map (void ((neighbor-map int*)
                                      (pos float4*)
                                      (n int)))
  (with-particle-index (p)
    (when (< p n)
      (with-cell-index ((i j k) (aref pos p))
        (let ((offset (offset i j k 0)))
          ;; Atomically increment the number of particles in the cell.
          (let ((l (atomic-add (pointer (aref neighbor-map offset)) 1)))
            ;; Set particle in the cell.
            (set (aref neighbor-map (offset i j k (+ l 1))) p)))))))

(defkernel clear-neighbor-map (void ((neighbor-map int*)))
  (let ((i (to-int (get-local-id 0)))
        (j (to-int (get-group-id 0)))
        (k (to-int (get-group-id 1))))
    (set (aref neighbor-map (offset i j k 0)) 0)))

(defkernelmacro do-neighbors ((var neighbor-map x) &body body)
  (with-gensyms (i0 j0 k0 i j k l)
    `(with-cell-index ((,i0 ,j0 ,k0) ,x)
       (do-range (,i (- ,i0 1) (+ ,i0 1))
         (do-range (,j (- ,j0 1) (+ ,j0 1))
           (do-range (,k (- ,k0 1) (+ ,k0 1))
             (do-range (,l 1 (aref ,neighbor-map (offset ,i ,j ,k 0)))
               (let ((,var (aref ,neighbor-map (offset ,i ,j ,k ,l))))
                 ,@body))))))))

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
                    (1+ capacity))))
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
                                     (vel float4*)
                                     (n int)))
  (with-particle-index (i)
    (when (< i n)
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
        (apply-accel-limit acc i)))))


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
                                 (n int)
                                 (neighbor-map int*)))
  (with-particle-index (i)
    (when (< i n)
      (let ((xi (aref pos i))
            (tmp 0.0))
        (do-neighbors (j neighbor-map xi)
          (let* ((xj (aref pos j))
                 (dr (* (- xi xj) simscale)))
            (when (<= (norm dr) h)
              (inc tmp (* pmass (poly6-kernel dr))))))
        (set (aref rho i) tmp)))))


;;
;; Update pressure

(defkernel update-pressure (void ((prs float*)
                                  (rho float*)
                                  (n int)))
  (with-particle-index (i)
    (when (< i n)
      (set (aref prs i) (* (- (aref rho i) restdensity)
                           intstiff)))))


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
                               (n int)
                               (neighbor-map int*)))
  (with-particle-index (i)
    (when (< i n)
      (let ((xi (aref pos i))
            (tmp (float4 0.0 0.0 0.0 0.0)))
        (do-neighbors (j neighbor-map xi)
          (when (/= i j)
            (let* ((xj (aref pos j))
                   (dr (* (- xi xj) simscale)))
              (when (<= (norm dr) h)
                (inc tmp (pressure-term rho prs i j dr))
                (inc tmp (viscosity-term vel rho i j dr))))))
        (set (aref force i) tmp)))))


;;
;; Update acceleration

(defkernel update-acceleration (void ((acc float4*)
                                      (force float4*)
                                      (rho float*)
                                      (n int)))
  (with-particle-index (i)
    (when (< i n)
      (set (aref acc i) (+ (/ (aref force i)
                              (aref rho i))
                           g)))))


;;
;; Update velocity

(defkernel update-velocity (void ((vel float4*)
                                  (acc float4*)
                                  (n int)))

  (with-particle-index (i)
    (when (< i n)
      (inc (aref vel i) (* (aref acc i) dt)))))


;;
;; Update position

(defkernel update-position (void ((pos float4*)
                                  (vel float4*)
                                  (n int)))
  (with-particle-index (i)
    (when (< i n)
      (inc (aref pos i) (/ (* (aref vel i) dt)
                           simscale)))))


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

(defun output-sphere (pos stream)
  (with-float4 (x y z w) pos
    (format stream +sphere-template+ x y z)))

(defun output (step pos)
  (format t "Output step ~A...~%" step)
  (let ((n (memory-block-size pos))
        (filename (filename step)))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (output-header out)
      (loop for i from 0 below n
         do (output-sphere (mem-aref pos i) out)))))


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

(defun peek-memory-block (memory-block)
  (sync-memory-block memory-block :device-to-host)
  (loop repeat 10
        for i from 0
     do (print (mem-aref memory-block i))))

#+nil
(defun main ()
  (let* (
         ;; Grid and block dims.
         (neighbor-map-grid-dim '(45 37 1))
         (neighbor-map-block-dim '(37 1 1))
         (particle-grid-dim '(512 1 1))
         (particle-block-dim '(64 1 1))
         ;; Get initial condition.
         (particles (initial-condition init-min init-max (/ pdist simscale)))
         ;; Get number of particles.
         (n (length particles))
         ;; Compute neighbor map origin.
         (origin (compute-origin box-min delta)))
    ;; Compute neighbor map size.
    (multiple-value-bind (size-x size-y size-z size)
        (compute-size box-min box-max delta capacity)
      ;; Set boundary condition and neighbor map.
      ;; With memory blocks.
      ; set-params kernel
    #+nil
      (with-memory-blocks ((pos 'float4 n)
                           (vel 'float4 n)
                           (acc 'float4 n)
                           (force 'float4 n)
                           (rho 'float n)
                           (prs 'float n)
                           (neighbor-map 'int size))
        ;; Print number of particles.
        (format t "~A particles~%" n)
        ;; Apply initial condition.
        (initialize pos vel particles)
        (sync-memory-block pos :host-to-device)
        (sync-memory-block vel :host-to-device)
                                        ;(output 0 pos)
        ;; Do simulation.
        (time
         (loop repeat 300
               for i from 1
               do ;; Clear neighbor map.
                  (clear-neighbor-map neighbor-map
                                      :grid-dim neighbor-map-grid-dim
                                      :block-dim neighbor-map-block-dim)
                  ;; Update neighbor map.
                  (update-neighbor-map neighbor-map pos n
                                       :grid-dim particle-grid-dim
                                       :block-dim particle-block-dim)
                  ;; Update density.
                  (update-density rho pos n neighbor-map
                                  :grid-dim particle-grid-dim
                                  :block-dim particle-block-dim)
                  ;; Update pressure.
                  (update-pressure prs rho n
                                   :grid-dim particle-grid-dim
                                   :block-dim particle-block-dim)
                  ;; Update force.
                  (update-force force pos vel rho prs n neighbor-map
                                :grid-dim particle-grid-dim
                                :block-dim particle-block-dim)
                  ;; Update acceleration.
                  (update-acceleration acc force rho n
                                       :grid-dim particle-grid-dim
                                       :block-dim particle-block-dim)
                  ;; Apply boundary condition.
                  (boundary-condition acc pos vel n
                                      :grid-dim particle-grid-dim
                                      :block-dim particle-block-dim)
                  ;; Update velocity.
                  (update-velocity vel acc n
                                   :grid-dim particle-grid-dim
                                   :block-dim particle-block-dim)
                  ;; Update position.
                  (update-position pos vel n
                                   :grid-dim particle-grid-dim
                                   :block-dim particle-block-dim)
                  ;; Synchronize CUDA context.
                  (synchronize-context)
                  ;; Output POV file.
                                        ;(when (= (mod i 10) 0)
                                        ;  (sync-memory-block pos :device-to-host)
                                        ;  (output (/ i 10) pos))
               ))))))

(defun foreign-float-to-lisp-float (foreign-array foreign-size)
  (let ((n (if (< foreign-size 100)
               foreign-size
               100)))
    (loop for i from 0 below n
          collecting (mem-aref foreign-array 'cl-float i))))

(defun pprint-foreign-float (foreign-array foreign-size)
  (pprint (foreign-float-to-lisp-float foreign-array foreign-size)))

(defun pprint-device-float (command-queue device size)
  (with-foreign-objects ((foreign-array 'cl-float size))
    (enqueue-read-buffer command-queue
                         device
                         +cl-true+
                         0
                         (* (foreign-type-size 'cl-float) size)
                         foreign-array)
    (pprint-foreign-float foreign-array size)))

(defun main ()
  (with-platform-id (platform)
    (with-device-ids (devices num-devices platform)
      (with-context (context (null-pointer) 1 devices)
        (let ((c-source-code (kernel-manager-translate *kernel-manager*))
              (device (mem-aref devices 'cl-device-id)))
          (with-program-with-source (program context 1 c-source-code)
            (build-program program 1 devices)
            (let* (;; Get initial condition.
                   (particles (initial-condition init-min init-max (/ pdist simscale)))
                   ;; Get number of particles.
                   (n (length particles))
                   ;; Compute neighbor map origin.
                   (origin (compute-origin box-min delta)))

              ;; Print number of particles.
              (format t "~A particles~%" n)
              (multiple-value-bind (size-x size-y size-z size)
                  (compute-size box-min box-max delta capacity)
                (with-foreign-objects ((pos :float (* 4 n))
                                       (vel :float (* 4 n)))
                  (initialize pos vel particles)
                  (with-buffers ((pos-device context +cl-mem-read-write+ (* 4 4 n))
                                 (vel-device context +cl-mem-read-write+ (* 4 4 n))
                                 (acc-device context +cl-mem-read-write+ (* 4 4 n))
                                 (force-device context +cl-mem-read-write+ (* 4 4 n))
                                 (rho-device context +cl-mem-read-write+ (* 4 n))
                                 (prs-device context +cl-mem-read-write+ (* 4 n))
                                 (neighbor-map-device context +cl-mem-read-write+ (* 4 size)))
                    (with-command-queue (command-queue context device 0)
                      (labels ((write-buffer (device size host)
                                 (enqueue-write-buffer command-queue
                                                       device
                                                       +cl-true+
                                                       0
                                                       size
                                                       host)))
                        (write-buffer pos-device (* 4 4 n) pos)
                        (write-buffer vel-device (* 4 4 n) vel)
                        (finish command-queue))

                      ;; Grid and block dims.
                      (with-work-sizes ((neighbor-map-global-work-size (* 45 37) 37)
                                        (neighbor-map-local-work-size 37)
                                        (particle-global-work-size (* 512 64))
                                        (particle-local-work-size 64))
                        ;; Do simulation.(time
                        (loop repeat 300
                              for i from 1
                              do ;; Clear neighbor map.
                                 (with-kernel (kernel program "oclcl_examples_sph_oclapi_clear_neighbor_map")
                                   (with-pointers ((neighbor-map-pointer neighbor-map-device))
                                     (set-kernel-arg kernel 0 8 neighbor-map-pointer)
                                     (enqueue-ndrange-kernel command-queue
                                                             kernel
                                                             1
                                                             neighbor-map-global-work-size
                                                             neighbor-map-local-work-size)
                                     (finish command-queue)))
                                 #+nil
                                 (clear-neighbor-map neighbor-map
                                                     :grid-dim neighbor-map-grid-dim
                                                     :block-dim neighbor-map-block-dim)
                                 #+nil
                                 (pprint-device-float command-queue neighbor-map-device size)

                                 ;; Update neighbor map.
                                 (with-kernel (kernel program "oclcl_examples_sph_oclapi_update_neighbor_map")
                                   (with-pointers ((neighbor-map-pointer neighbor-map-device)
                                                   (pos-pointer pos-device))
                                     (with-foreign-object (n-pointer 'cl-int)
                                       (setf (mem-aref n-pointer 'cl-int) n)
                                       (set-kernel-arg kernel 0 8 neighbor-map-pointer)
                                       (set-kernel-arg kernel 1 8 pos-pointer)
                                       (set-kernel-arg kernel 2 4 n-pointer)
                                       (enqueue-ndrange-kernel command-queue
                                                               kernel
                                                               1
                                                               particle-global-work-size
                                                               particle-local-work-size)
                                       (finish command-queue))))
                                 #+nil
                                 (update-neighbor-map neighbor-map pos n
                                                      :grid-dim particle-grid-dim
                                                      :block-dim particle-block-dim)

                                 ;; Update density.
                                 #+nil
                                 (with-kernel (kernel program "oclcl_examples_sph_oclapi_update_density")
                                   (with-pointers ((rho-pointer rho-device)
                                                   (pos-pointer pos-device)
                                                   (neighbor-map-pointer neighbor-map-device))
                                     (with-foreign-object (n-pointer 'cl-int)
                                       (setf (mem-aref n-pointer 'cl-int) n)
                                       (set-kernel-arg kernel 0 8 rho-pointer)
                                       (set-kernel-arg kernel 1 8 pos-pointer)
                                       (set-kernel-arg kernel 2 4 n-pointer)
                                       (set-kernel-arg kernel 3 8 neighbor-map-pointer)
                                       (enqueue-ndrange-kernel command-queue
                                                               kernel
                                                               1
                                                               particle-global-work-size
                                                               particle-local-work-size)
                                       (finish command-queue))))
                                 #+nil
                                 (update-density rho pos n neighbor-map
                                                 :grid-dim particle-grid-dim
                                                 :block-dim particle-block-dim)

                                 ;; Update pressure.
                                 (with-kernel (kernel program "oclcl_examples_sph_oclapi_update_pressure")
                                   (with-pointers ((rho-pointer rho-device)
                                                   (prs-pointer prs-device))
                                     (with-foreign-object (n-pointer 'cl-int)
                                       (setf (mem-aref n-pointer 'cl-int) n)
                                       (set-kernel-arg kernel 0 8 prs-pointer)
                                       (set-kernel-arg kernel 1 8 rho-pointer)
                                       (set-kernel-arg kernel 2 4 n-pointer)
                                       (enqueue-ndrange-kernel command-queue
                                                               kernel
                                                               1
                                                               particle-global-work-size
                                                               particle-local-work-size)
                                       (finish command-queue))))
                                 #+nil
                                 (update-pressure prs rho n
                                                  :grid-dim particle-grid-dim
                                                  :block-dim particle-block-dim)


                                 ;; Update force.
                                 #+nil
                                 (with-kernel (kernel program "oclcl_examples_sph_oclapi_update_force")
                                   (with-pointers ((force-pointer force-device)
                                                   (pos-pointer pos-device)
                                                   (vel-pointer vel-device)
                                                   (rho-pointer rho-device)
                                                   (prs-pointer prs-device)
                                                   (neighbor-map-pointer neighbor-map-device))
                                     (with-foreign-object (n-pointer 'cl-int)
                                       (setf (mem-aref n-pointer 'cl-int) n)
                                       (set-kernel-arg kernel 0 8 force-pointer)
                                       (set-kernel-arg kernel 1 8 prs-pointer)
                                       (set-kernel-arg kernel 2 8 vel-pointer)
                                       (set-kernel-arg kernel 3 8 rho-pointer)
                                       (set-kernel-arg kernel 4 8 prs-pointer)
                                       (set-kernel-arg kernel 5 4 n-pointer)
                                       (set-kernel-arg kernel 6 8 neighbor-map-pointer)
                                       (enqueue-ndrange-kernel command-queue
                                                               kernel
                                                               1
                                                               particle-global-work-size
                                                               particle-local-work-size)
                                       (finish command-queue))))
                                 #+nil
                                 (update-force force pos vel rho prs n neighbor-map
                                               :grid-dim particle-grid-dim
                                               :block-dim particle-block-dim)

                                 ;; Update acceleration.
                                 #+nil
                                 (with-kernel (kernel program "oclcl_examples_sph_oclapi_update_acceleration")
                                   (with-pointers ((acc-pointer acc-device)
                                                   (force-pointer force-device)
                                                   (rho-pointer rho-device))
                                     (with-foreign-object (n-pointer 'cl-int)
                                       (setf (mem-aref n-pointer 'cl-int) n)
                                       (set-kernel-arg kernel 0 8 acc-pointer)
                                       (set-kernel-arg kernel 1 8 force-pointer)
                                       (set-kernel-arg kernel 2 8 rho-pointer)
                                       (set-kernel-arg kernel 3 4 n-pointer)
                                       (enqueue-ndrange-kernel command-queue
                                                               kernel
                                                               1
                                                               particle-global-work-size
                                                               particle-local-work-size)
                                       (finish command-queue))))
                                 #+nil
                                 (update-acceleration acc force rho n
                                                      :grid-dim particle-grid-dim
                                                      :block-dim particle-block-dim)

                                 ;; Apply boundary condition.
                                 #+nil
                                 (with-kernel (kernel program "oclcl_examples_sph_oclapi_boundary_condition")
                                   (with-pointers ((acc-pointer acc-device)
                                                   (pos-pointer pos-device)
                                                   (vel-pointer vel-device))
                                     (with-foreign-object (n-pointer 'cl-int)
                                       (setf (mem-aref n-pointer 'cl-int) n)
                                       (set-kernel-arg kernel 0 8 acc-pointer)
                                       (set-kernel-arg kernel 1 8 pos-pointer)
                                       (set-kernel-arg kernel 2 8 vel-pointer)
                                       (set-kernel-arg kernel 3 4 n-pointer)
                                       (enqueue-ndrange-kernel command-queue
                                                               kernel
                                                               1
                                                               particle-global-work-size
                                                               particle-local-work-size)
                                       (finish command-queue))))
                                 #+nil
                                 (boundary-condition acc pos vel n
                                                     :grid-dim particle-grid-dim
                                                     :block-dim particle-block-dim)

                                 ;; Update velocity.
                                 (with-kernel (kernel program "oclcl_examples_sph_oclapi_update_velocity")
                                   (with-pointers ((vel-pointer vel-device)
                                                   (acc-pointer acc-device))
                                     (with-foreign-object (n-pointer 'cl-int)
                                       (setf (mem-aref n-pointer 'cl-int) n)
                                       (set-kernel-arg kernel 0 8 vel-pointer)
                                       (set-kernel-arg kernel 1 8 acc-pointer)
                                       (set-kernel-arg kernel 2 4 n-pointer)
                                       (enqueue-ndrange-kernel command-queue
                                                               kernel
                                                               1
                                                               particle-global-work-size
                                                               particle-local-work-size)
                                       (finish command-queue))))
                                 #+nil
                                 (update-velocity vel acc n
                                                  :grid-dim particle-grid-dim
                                                  :block-dim particle-block-dim)

                                 ;; Update position.
                                 (with-kernel (kernel program "oclcl_examples_sph_oclapi_update_position")
                                   (with-pointers ((pos-pointer pos-device)
                                                   (vel-pointer vel-device))
                                     (with-foreign-object (n-pointer 'cl-int)
                                       (setf (mem-aref n-pointer 'cl-int) n)
                                       (set-kernel-arg kernel 0 8 pos-pointer)
                                       (set-kernel-arg kernel 1 8 vel-pointer)
                                       (set-kernel-arg kernel 2 4 n-pointer)
                                       (enqueue-ndrange-kernel command-queue
                                                               kernel
                                                               1
                                                               particle-global-work-size
                                                               particle-local-work-size)
                                       (finish command-queue))))
                                 #+nil
                                 (update-position pos vel n
                                                  :grid-dim particle-grid-dim
                                                  :block-dim particle-block-dim)

                                 ;; Synchronize CUDA context.
                                 #+nil
                                 (synchronize-context)
                                 ;; Output POV file.
                                        ;(when (= (mod i 10) 0)
                                        ;  (sync-memory-block pos :device-to-host)
                                        ;  (output (/ i 10) pos))
                              )))))))))))))
