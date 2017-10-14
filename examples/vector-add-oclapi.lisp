#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2016 gos-k (mag4.elan@gmail.com)
|#

#|
  This file is based on the CUDA SDK's "vectorAdd" sample.
|#

(in-package :cl-user)
(defpackage oclcl-examples.vector-add-oclapi
  (:use :cl
        :cffi
        :oclcl)
  (:import-from :cl-oclapi
                :with-platform-id
                :with-device-ids
                :with-context
                :with-program-with-source
                :with-command-queue
                :with-buffer
                :with-buffers
                :with-work-size
                :with-kernel
                :with-pointers
                :cl-size
                :cl-device-id
                :enqueue-read-buffer
                :enqueue-write-buffer
                :set-kernel-arg
                :enqueue-ndrange-kernel
                :build-program
                :finish
                :+cl-true+
                :+cl-mem-read-only+
                :+cl-mem-write-only+)
  (:export :main))
(in-package :oclcl-examples.vector-add-oclapi)

(define-kernel-module :vector-add-oclapi)
(in-kernel-module :vector-add-oclapi)

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
  (with-platform-id (platform)
    (with-device-ids (devices num-devices platform)
      (with-context (context (null-pointer) 1 devices)
        (let ((c-source-code (kernel-manager-translate *kernel-manager*))
              (device (mem-aref devices 'cl-device-id)))
          (with-program-with-source (program context 1 c-source-code)
            (build-program program 1 devices)
            (let* ((elements 128)
                   (float-size (foreign-type-size :float))
                   (data-bytes (* float-size elements)))
              (with-foreign-objects ((a-host :float elements)
                                     (b-host :float elements)
                                     (c-host :float elements))
                (random-init a-host elements)
                (random-init b-host elements)
                (zero-init c-host elements)
                (with-buffers ((a-device context +cl-mem-read-only+ data-bytes)
                               (b-device context +cl-mem-read-only+ data-bytes)
                               (c-device context +cl-mem-write-only+ data-bytes))
                  (with-command-queue (command-queue context device 0)
                    (enqueue-write-buffer command-queue
                                          a-device
                                          +cl-true+
                                          0
                                          data-bytes
                                          a-host)
                    (enqueue-write-buffer command-queue
                                          b-device
                                          +cl-true+
                                          0
                                          data-bytes
                                          b-host)
                    (finish command-queue)
                    (with-work-size (global-work-size elements)
                      (with-kernel (kernel program (kernel-manager-function-c-name *kernel-manager*
                                                                                   'vec-add-kernel))
                        (with-pointers ((a-pointer a-device)
                                        (b-pointer b-device)
                                        (c-pointer c-device))
                          (set-kernel-arg kernel 0 8 a-pointer)
                          (set-kernel-arg kernel 1 8 b-pointer)
                          (set-kernel-arg kernel 2 8 c-pointer)
                          (enqueue-ndrange-kernel command-queue
                                                  kernel
                                                  1
                                                  global-work-size
                                                  (null-pointer))
                          (enqueue-read-buffer command-queue
                                               c-device
                                               +cl-true+
                                               0
                                               data-bytes
                                               c-host)
                          (finish command-queue)
                          (verify-result a-host b-host c-host elements))))))))))))))
