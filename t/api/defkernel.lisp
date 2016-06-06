#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.api.defkernel
  (:use :cl :prove
        :oclcl.api.defkernel
        :oclcl.lang)
  (:import-from :oclcl.api.defkernel)
  (:import-from :oclcl.api.kernel-manager
                :make-kernel-manager
                :kernel-manager-translate
                :*kernel-manager*))
(in-package :oclcl-test.api.defkernel)

(plan nil)

(defmacro with-stub-kernel-manager (&body body)
  `(let ((kernel-manager *kernel-manager*))
    (setf *kernel-manager* (make-kernel-manager))
    (unwind-protect
         (progn ,@body)
      (progn
        (setf *kernel-manager* kernel-manager)
        t))))

;;;
;;; test DEFKERNEL macro
;;;

(diag "DEFKERNEL")

;; test "let1" kernel
(defkernel let1 (void ())
  (let ((i 0))
    (return))
  (let ((i 0))))

;(with-cuda (0)
;  (is (let1 :grid-dim (list 1 1 1)
;            :block-dim (list 1 1 1))
;      nil "basic case 1"))

;; test "use-one" kernel
;(defkernel one (int ())
;  (return 1))
;
;(defkernel use-one (void ())
;  (let ((i (one)))
;    (return)))

;(with-cuda (0)
;  (is (use-one :grid-dim (list 1 1 1)
;               :block-dim (list 1 1 1))
;      nil "basic case 2"))


;; test "argument" kernel
(defkernel argument (void ((i int) (j float3)))
  (return))

;(with-cuda (0)
;  (is (argument 1 (make-float3 0.0 0.0 0.0) :grid-dim (list 1 1 1)
;                                            :block-dim (list 1 1 1))
;      nil "basic case 3"))

;; test "kernel-bool" kernel
(defkernel kernel-bool (void ((a bool*)))
  (set (aref a 0) t)
  (set (aref a 1) nil)
  (return))

;(with-cuda (0)
;  (with-memory-blocks ((a 'bool 2))
;    (setf (memory-block-aref a 0) nil
;          (memory-block-aref a 1) t)
;    (sync-memory-block a :host-to-device)
;    (is (kernel-bool a :grid-dim '(1 1 1) :block-dim '(1 1 1))
;        nil "basic case 4")
;    (sync-memory-block a :device-to-host)
;    (is (memory-block-aref a 0) t
;        "basic case 5")
;    (is (memory-block-aref a 1) nil
;        "basic case 6")))

;; test "kernel-float3" kernel
(defkernel kernel-float3 (void ((a float*) (x float3)))
  (set (aref a 0) (+ (float3-x x) (float3-y x) (float3-z x))))

;(let ((x (make-float3 1.0 2.0 3.0)))
;  (with-cuda (0)
;    (with-memory-blocks ((a 'float 1))
;      (setf (memory-block-aref a 0) 1.0)
;      (sync-memory-block a :host-to-device)
;      (is (kernel-float3 a x :grid-dim '(1 1 1) :block-dim '(1 1 1))
;          nil "basic case 7")
;      (sync-memory-block a :device-to-host)
;      (is (memory-block-aref a 0) 6.0
;          "basic case 8"))))

;; test DO statement
(defkernel test-do-kernel (void ((x int*)))
  (do ((i 0 (+ i 1)))
      ((> i 15))
    (set (aref x 0) (+ (aref x 0) 1))))

#|
(with-cuda (0)
  (with-memory-blocks ((x 'int 1))
    (setf (memory-block-aref x 0) 0)
    (sync-memory-block x :host-to-device)
    (is (test-do-kernel x :grid-dim '(1 1 1) :block-dim '(1 1 1))
        nil "basic case 9")
    (sync-memory-block x :device-to-host)
    (is (memory-block-aref x 0) 16
        "basic case 10")))

;; test multi-argument arithmetic
(defkernel test-add (void ((x int*)))
  (set (aref x 0) (+ 1 1 1)))

(with-cuda (0)
  (with-memory-blocks ((x 'int 1))
    (setf (memory-block-aref x 0) 0)
      (sync-memory-block x :host-to-device)
      (is (test-add x :grid-dim '(1 1 1) :block-dim '(1 1 1))
          nil "basic case 11")
      (sync-memory-block x :device-to-host)
      (is (memory-block-aref x 0) 3
          "basic case 12")))

;; test atomic function
(defkernel test-atomic-add (void ((x int*)))
  (atomic-add (pointer (aref x 0)) 1))

(defkernel test-no-atomic-add (void ((x int*)))
  (set (aref x 0) (+ (aref x 0) 1)))

(with-cuda (0)
  (with-memory-blocks ((x 'int 1)
                       (y 'int 1))
    (setf (memory-block-aref x 0) 0
          (memory-block-aref y 0) 0)
    (sync-memory-block x :host-to-device)
    (sync-memory-block y :host-to-device)
    (is (test-atomic-add x :grid-dim '(1 1 1) :block-dim '(256 1 1))
        nil "basic case 13")
    (is (test-no-atomic-add y :grid-dim '(1 1 1) :block-dim '(256 1 1))
        nil "basic case 14")
    (sync-memory-block x :device-to-host)
    (sync-memory-block y :host-to-device)
    (is (memory-block-aref x 0) 256
        "basic case 15")
    (isnt (memory-block-aref y 0) 256
          "basic case 16")))

;; test built-in vector type
(defkernel test-float3-add (void ((x float3*)))
  (set (aref x 0) (+ (aref x 0) (float3 1.0 1.0 1.0))))

(with-cuda (0)
  (with-memory-blocks ((x 'float3 1))
    (setf (memory-block-aref x 0) (make-float3 0.0 0.0 0.0))
    (sync-memory-block x :host-to-device)
    (is (test-float3-add x :grid-dim '(1 1 1) :block-dim '(1 1 1))
        nil "basic case 17")
    (sync-memory-block x :device-to-host)
    (is (memory-block-aref x 0) (make-float3 1.0 1.0 1.0) :test #'float3-=
        "basic case 18")))

;; test launching with passing a raw device-ptr
(defkernel test-device-ptr (void ((x int*)))
  (set (aref x 0) 1))

(with-cuda (0)
  (with-host-memory (h 'int 1)
    (with-device-memory (d 'int 1)
      (setf (cffi:mem-aref h (cffi-type 'int) 0) 0)
      (memcpy-host-to-device d h 'int 1)
      (test-device-ptr d :grid-dim '(1 1 1) :block-dim '(1 1 1))
      (memcpy-device-to-host h d 'int 1)
      (is (cffi:mem-aref h (cffi-type 'int) 0) 1
          "basic case 19"))))
|#


;;; test DEFMEMORY macro
;;;

(subtest "DEFMEMORY"
  (with-stub-kernel-manager
    (defmemory a 42 :constant)
    (defmemory b 0 :global)
    (is (kernel-manager-translate *kernel-manager*)
"

/**
 *  Kernel globals
 */

__constant int oclcl_test_api_defkernel_a = 42;
__global int oclcl_test_api_defkernel_b = 0;


/**
 *  Kernel function prototypes
 */



/**
 *  Kernel function definitions
 */

" "work defmemory")))

;;;
;;; test DEFKERNELMACRO macro
;;;

(subtest "DEFKERNELMACRO"
  (with-stub-kernel-manager

    (defkernelmacro when (test &body forms)
      `(if ,test
           (progn ,@forms)))

    (defkernel test-when (void ())
      (when t (return))
      (return))

    (is (kernel-manager-translate *kernel-manager*)
"

/**
 *  Kernel globals
 */



/**
 *  Kernel function prototypes
 */

__kernel void oclcl_test_api_defkernel_test_when();


/**
 *  Kernel function definitions
 */

__kernel void oclcl_test_api_defkernel_test_when()
{
  if (true) {
    return;
  }
  return;
}
" "work defkernelmacro")))

;;;
;;; test DEFKERNEL-SYMBOL-MACRO macro
;;;

(subtest "DEFKERNEL-SYMBOL-MACRO"
  (with-stub-kernel-manager

    (defkernel-symbol-macro x 1)

    (defkernel test-symbol-macro (void ((ret int*)))
      (set (aref ret 0) x)
      (return))

    (is (kernel-manager-translate *kernel-manager*)
"

/**
 *  Kernel globals
 */



/**
 *  Kernel function prototypes
 */

__kernel void oclcl_test_api_defkernel_test_symbol_macro( __global int* ret );


/**
 *  Kernel function definitions
 */

__kernel void oclcl_test_api_defkernel_test_symbol_macro( __global int* ret )
{
  ret[0] = 1;
  return;
}
" "work defkernel-symbol-macro")))


#|

;;;
;;; test MOD
;;;
(defkernel test-mod (void ((x int*)))
  (set (aref x 0) (mod (aref x 0) 5)))

(with-cuda (0)
  (with-memory-blocks ((x 'int 1))
    (setf (memory-block-aref x 0) 7)
    (sync-memory-block x :host-to-device)
    (is (test-mod x :grid-dim '(1 1 1) :block-dim '(1 1 1))
        nil "basic case 22")
    (sync-memory-block x :device-to-host)
    (is (memory-block-aref x 0) 2
        "basic case 23")))
|#


;;;
;;; test EXPAND-MACRO function
;;;

(subtest "EXPAND-MACRO"
  (defkernelmacro foo (x)
    `(return ,x))

  (defkernelmacro bar (x)
    `(foo ,x))

  (defkernel-symbol-macro a 1.0)

  (defkernel-symbol-macro b a)

  (is-values (expand-macro-1 '(foo 1)) '((return 1) t))
  (is-values (expand-macro-1 '(bar 1)) '((foo 1) t))
  (is-values (expand-macro-1 '(baz 1)) '((baz 1) nil))
  (is-values (expand-macro-1 'a) '(1.0 t))
  (is-values (expand-macro-1 'b) '(a t))
  (is-values (expand-macro-1 'c) '(c nil))
  (is-error (expand-macro-1 '(foo)) error)

  (is-values (expand-macro '(foo 1)) '((return 1) t))
  (is-values (expand-macro '(bar 1)) '((return 1) t))
  (is-values (expand-macro '(baz 1)) '((baz 1) nil))
  (is-values (expand-macro 'a) '(1.0 t))
  (is-values (expand-macro 'b) '(1.0 t))
  (is-values (expand-macro 'c) '(c nil))
  (is-error (expand-macro '(foo)) error))


(finalize)
