#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.api.defkernel
  (:use :cl :prove
        :oclcl.api.defkernel
        :oclcl.lang
        :oclcl.lang.program
        :oclcl.lang.compiler.compile-program)
  (:import-from :oclcl.api.defkernel))
(in-package :oclcl-test.api.defkernel)

(plan nil)

(defmacro with-stub-program (&body body)
  `(let ((oclcl.lang.program:*program* (oclcl.lang.program:make-program)))
     ,@body))

;;;
;;; test DEFKERNEL macro
;;;

(diag "DEFKERNEL")

;; test "let1" kernel
(defkernel let1 (void ())
  (let ((i 0))
    (return))
  (let ((i 0))))

; test "use-one" kernel
(defkernel one (int ())
  (return 1))

(defkernel use-one (void ())
  (let ((i (one)))
    (return)))

;; test "argument" kernel
(defkernel argument (void ((i int) (j float3)))
  (return))

;; test "kernel-bool" kernel
(defkernel kernel-bool (void ((a bool*)))
  (set (aref a 0) t)
  (set (aref a 1) nil)
  (return))

;; test "kernel-float3" kernel
(defkernel kernel-float3 (void ((a float*) (x float3)))
  (set (aref a 0) (+ (float3-x x) (float3-y x) (float3-z x))))

;; test DO statement
(defkernel test-do-kernel (void ((x int*)))
  (do ((i 0 (+ i 1)))
      ((> i 15))
    (set (aref x 0) (+ (aref x 0) 1))))

;;; test DEFMEMORY macro
;;;

(subtest "DEFMEMORY"
  (with-stub-program
    (defmemory a 42 :constant)
    (defmemory b 0 :global)
    (is (compile-program *program*)
"

/**
 *  Memory objects
 */

__constant int oclcl_test_api_defkernel_a = 42;
__global int oclcl_test_api_defkernel_b = 0;




" "work defmemory")))

;;;
;;; test DEFKERNELMACRO macro
;;;

(subtest "DEFKERNELMACRO"
  (with-stub-program

    (defkernelmacro when (test &body forms)
      `(if ,test
           (progn ,@forms)))

    (defkernel test-when (void ())
      (when t (return))
      (return))

    (is (compile-program *program*)
"



/**
 *  Kernel function prototypes
 */

__kernel void oclcl_test_api_defkernel_test_when();


/**
 *  Kernel function definitions
 */

__kernel void oclcl_test_api_defkernel_test_when()
{
  if (true)
  {
    return;
  }
  return;
}
" "work defkernelmacro")))

;;;
;;; test DEFKERNEL-SYMBOL-MACRO macro
;;;

(subtest "DEFKERNEL-SYMBOL-MACRO"
  (with-stub-program

    (defkernel-symbol-macro x 1)

    (defkernel test-symbol-macro (void ((ret int*)))
      (set (aref ret 0) x)
      (return))

    (is (compile-program *program*)
"



/**
 *  Kernel function prototypes
 */

__kernel void oclcl_test_api_defkernel_test_symbol_macro(__global int* ret);


/**
 *  Kernel function definitions
 */

__kernel void oclcl_test_api_defkernel_test_symbol_macro(__global int* ret)
{
  ret[0] = 1;
  return;
}
" "work defkernel-symbol-macro")))


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
