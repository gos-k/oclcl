#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2016 gos-k (mag4.elan@gmail.com)
|#

#|
  This file is based on the CUDA SDK's "vectorAdd" sample.
|#

(in-package :cl-user)
(defpackage oclcl-examples.vector-add
  (:use :cl
        :oclcl
        :cffi
        :eazy-opencl.host)
  (:export :main))
(in-package :oclcl-examples.vector-add)

(define-program :vector-add-eazyopencl
  (:use :oclcl))
(in-program :vector-add-eazyopencl)

(defun random-init (data n)
  (dotimes (i n)
    (let ((r (random 1.0)))
      (setf (mem-aref data :float i) r))))

(defun zero-init (data n)
  (dotimes (i n)
    (setf (mem-aref data :float i) 0.0)))

(defun verify-result (as bs cs n)
  (dotimes (i n)
    (let ((a (mem-aref as :float i))
          (b (mem-aref bs :float i))
          (c (mem-aref cs :float i)))
      (let ((sum (+ a b)))
        (when (> (abs (- c sum)) 1.0)
          (error (format nil "verification fault, i:~A a:~A b:~A c:~A"
                         i a b c))))))
  (format t "verification succeed.~%"))

(defkernel vec-add-kernel (void ((a float*) (b float*) (c float*)))
  (let ((i (get-global-id 0)))
    (set (aref c i)
         (+ (aref a i) (aref b i)))))

(defvar *vector-add* "

__kernel void oclcl_examples_vector_add_vec_add_kernel( __global float* a, __global float* b, __global float* c )
{
    size_t i = get_global_id( 0 );
    c[i] = (a[i] + b[i]);
}

")

(defun main ()
  (let* ((platform-id (car (get-platform-ids)))
         (devices (get-device-ids platform-id :device-type-default))
         (context (create-context devices))
         (command-queue (create-command-queue context (first devices) 0))
         (c-source-code (compile-program *program*))
         (program
           (create-program-with-source context c-source-code)
           #+nil
           (create-program-with-source context *vector-add*))
         (elements 128)
         (float-size (foreign-type-size :float))
         (data-bytes (* float-size elements)))
    (with-foreign-objects ((a-host :float elements)
                           (b-host :float elements)
                           (c-host :float elements))
      (random-init a-host elements)
      (random-init b-host elements)
      (zero-init c-host elements)
      (let* ((a-device (create-buffer context :mem-read-only data-bytes))
             (b-device (create-buffer context :mem-read-only data-bytes))
             (c-device (create-buffer context :mem-read-only data-bytes)))
        (%ocl:enqueue-write-buffer command-queue
                                   a-device
                                   %ocl:true
                                   0
                                   data-bytes
                                   a-host
                                   0
                                   (null-pointer)
                                   (null-pointer))
        (%ocl:enqueue-write-buffer command-queue
                                   b-device
                                   %ocl:true
                                   0
                                   data-bytes
                                   b-host
                                   0
                                   (null-pointer)
                                   (null-pointer))
        (%ocl/h::with-foreign-array (global-work-size '%ocl:size-t (list elements))
          (build-program program :devices devices)
          (let ((kernel
                  (create-kernel program "oclcl_examples_vector_add_vec_add_kernel")
                  #+nil
                  (create-kernel program "hello")))
            (set-kernel-arg kernel 0 a-device '%ocl:mem)
            (set-kernel-arg kernel 1 b-device '%ocl:mem)
            (set-kernel-arg kernel 2 c-device '%ocl:mem)
            (%ocl:enqueue-nd-range-kernel command-queue
                                          kernel
                                          1
                                          (null-pointer)
                                          global-work-size
                                          (null-pointer)
                                          0
                                          (null-pointer)
                                          (null-pointer))))
        (%ocl:enqueue-read-buffer command-queue
                                  c-device
                                  %ocl:true
                                  0
                                  data-bytes
                                  c-host
                                  0
                                  (null-pointer)
                                  (null-pointer)))
      (verify-result a-host b-host c-host elements))))
