#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage :oclcl.tests.api.defkernel
  (:use :cl :rove
        :oclcl.tests.utils
        :oclcl.api.defkernel
        :oclcl.lang
        :oclcl.lang.program
        :oclcl.lang.compiler.compile-program)
  (:import-from :oclcl.api.defkernel))
(in-package :oclcl.tests.api.defkernel)

(defmacro with-stub-program (&body body)
  `(let ((oclcl.lang.program:*program* (oclcl.lang.program:make-program)))
     ,@body))

;;;
;;; test DEFKERNEL macro
;;;

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

(deftest defmemory
  (with-stub-program
    (defmemory a 42 :constant)
    (defmemory b 0 :global)
    (is (compile-program *program*)
"

/**
 *  Memory objects
 */

__constant int oclcl_tests_api_defkernel_a = 42;
__global int oclcl_tests_api_defkernel_b = 0;




" "work defmemory")))

;;;
;;; test DEFKERNELMACRO macro
;;;

(deftest defkernelmacro
  (with-stub-program

    (defkernelmacro alfa (test &body forms)
      `(if ,test
           (progn ,@forms)))

    (defkernel test-alfa (void ())
      (alfa t (return))
      (return))

    (is (compile-program *program*)
"



/**
 *  Kernel function prototypes
 */

__kernel void oclcl_tests_api_defkernel_test_alfa();


/**
 *  Kernel function definitions
 */

__kernel void oclcl_tests_api_defkernel_test_alfa()
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

(deftest defkernel-symbol-macro
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

__kernel void oclcl_tests_api_defkernel_test_symbol_macro(__global int* ret);


/**
 *  Kernel function definitions
 */

__kernel void oclcl_tests_api_defkernel_test_symbol_macro(__global int* ret)
{
  ret[0] = 1;
  return;
}
" "work defkernel-symbol-macro")))


;;;
;;; test EXPAND-MACRO function
;;;

(deftest expand-macro
  (defkernelmacro foo (x)
    `(return ,x))

  (defkernelmacro bar (x)
    `(foo ,x))

  (defkernel-symbol-macro a 1.0)

  (defkernel-symbol-macro b a)

  (testing "exapnd-macro-1"
    (is-values (expand-macro-1 '(foo 1)) '((return 1) t))
    (is-values (expand-macro-1 '(bar 1)) '((foo 1) t))
    (is-values (expand-macro-1 '(baz 1)) '((baz 1) nil))
    (is-values (expand-macro-1 'a) '(1.0 t))
    (is-values (expand-macro-1 'b) '(a t))
    (is-values (expand-macro-1 'c) '(c nil))
    (ok (signals (expand-macro-1 '(foo)) 'error)))

  (testing "exapnd-macro"
    (is-values (expand-macro '(foo 1)) '((return 1) t))
    (is-values (expand-macro '(bar 1)) '((return 1) t))
    (is-values (expand-macro '(baz 1)) '((baz 1) nil))
    (is-values (expand-macro 'a) '(1.0 t))
    (is-values (expand-macro 'b) '(1.0 t))
    (is-values (expand-macro 'c) '(c nil))
    (ok (signals (expand-macro '(foo)) 'error))))
