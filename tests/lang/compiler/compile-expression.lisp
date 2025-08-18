#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.tests.lang.compiler.compile-expression
  (:use :cl :rove
        :oclcl.tests.utils
        :oclcl.lang.syntax
        :oclcl.lang.data
        :oclcl.lang.type
        :oclcl.lang.built-in
        :oclcl.lang.environment
        :oclcl.lang.compiler.compile-expression)
  (:import-from :oclcl.lang.compiler.compile-expression
                :compile-macro
                :compile-symbol-macro
                :compile-literal
                :compile-opencl-literal
                :compile-reference
                :compile-inline-if
                :compile-arithmetic
                :compile-function))
(in-package :oclcl.tests.lang.compiler.compile-expression)

;;;
;;; test COMPILE-EXPRESSION function
;;;

(deftest compile-expression
  (with-empty-env (var-env func-env)
    (is (compile-expression 1 var-env func-env) "1")))


;;;
;;; test COMPILE-MACRO function
;;;

(deftest compile-macro
  (with-empty-env (var-env func-env)
    (setf func-env (function-environment-add-macro 'foo '(x) '(`(+ ,x ,x)) func-env))
    (is (compile-macro '(foo 1) var-env func-env) "(1 + 1)"
        "basic case 1")))


;;;
;;; test COMPILE-SYMBOL-MACRO function
;;;

(deftest compile-symbol-macro
  (with-empty-env (var-env func-env)
    (setf var-env (variable-environment-add-symbol-macro 'x 1 var-env))
    (is (compile-symbol-macro 'x var-env func-env) "1"
        "basic case 1")))


;;;
;;; test COMPILE-LITERAL function
;;;

(deftest compile-literal

  (is (compile-literal t) "true"
      "basic case 1")

  (is (compile-literal nil) "false"
      "basic case 2")

  (is (compile-literal 1) "1"
      "basic case 3")

  (is (compile-literal 1.0f0) "1.0f"
      "basic case 4")

  (is (compile-literal 1.0d0) "1.0"
      "basic case 5")

  (is (compile-literal "literal") "\"literal\""
      "string literal"))

;;; test COMPILE-OPENCL-LITERAL function

(deftest compile-opencl-literal
  (is (compile-opencl-literal :clk-local-mem-fence)
      "CLK_LOCAL_MEM_FENCE"
      "CLK_LOCAL_MEM_FENCE")
  (is (compile-opencl-literal :clk-global-mem-fence)
      "CLK_GLOBAL_MEM_FENCE"
      "CLK_GLOBAL_MEM_FENCE"))

;;;
;;; test COMPILE-REFERENCE funcion
;;;

(deftest compile-reference-variable
  (let* ((add-var (variable-environment-add-variable 'x 'int (empty-variable-environment)))
         (add-symbol-macro (variable-environment-add-symbol-macro 'y 'y-expansion add-var))
         (var-env (variable-environment-add-memory 'z 'int 1 (variable-environment-add-variable 'y-expansion 'float add-symbol-macro)))
         (func-env (empty-function-environment)))
    (is (compile-reference 'x var-env func-env) "x"
        "basic case 1")
    (is (compile-reference 'z var-env func-env)
        "oclcl_tests_lang_compiler_compile_expression_z"
        "basic case 2")
    (ok (signals (compile-reference 'y var-env func-env) 'simple-error)
        "FORM which is a variable not found.")
    (ok (signals (compile-reference 'a var-env func-env) 'simple-error)
        "FORM which is a variable not found.")))

(deftest compile-reference-structure
  (let ((var-env (variable-environment-add-variable 'x 'float3
                                                    (empty-variable-environment)))
        (func-env (empty-function-environment)))
    (is (compile-reference '(float3-x x) var-env func-env) "x.x"
        "basic case 1")
    (is (compile-reference '(float3-y x) var-env func-env) "x.y"
        "basic case 2")
    (ok (signals (compile-reference '(float4-x x) var-env func-env)
                 'simple-error))))


(deftest compile-reference-array
  (let* ((add-var (variable-environment-add-variable 'x 'int* (empty-variable-environment)))
         (var-env (variable-environment-add-variable 'i 'int add-var))
         (func-env (empty-function-environment)))
    (is (compile-reference '(aref x i) var-env func-env) "x[i]"
        "basic case 1")))


;;;
;;; test COMPILE-INLINE-IF function
;;;

(deftest compile-inline-if
  (with-empty-env (var-env func-env)
    (is (compile-inline-if '(if (= 1 1) 1 2) var-env func-env)
        "((1 == 1) ? 1 : 2)"
        "basic case 1")))


;;;
;;; test COMPILE-ARITHMETIC function
;;;

(deftest compile-arithmetic
  (with-empty-env (var-env func-env)
    (is (compile-arithmetic '(+ 1 1 1) var-env func-env) "((1 + 1) + 1)"
        "add integer")
    (is (compile-arithmetic '(- 1 1 1) var-env func-env) "((1 - 1) - 1)"
        "sub integer")
    (is (compile-arithmetic '(* 1 1 1) var-env func-env) "((1 * 1) * 1)"
        "mul integer")
    (is (compile-arithmetic '(/ 1 1 1) var-env func-env) "((1 / 1) / 1)"
        "div integer")
    (is (compile-arithmetic '(mod 1 1 1) var-env func-env) "((1 % 1) % 1)"
        "mod integer")
    (is (compile-arithmetic '(+ 1 1 (- 1 1)) var-env func-env) "((1 + 1) + (1 - 1))"
        "mix integer")
    (is (compile-arithmetic '(+ 1.0f0 2.0f0 3.0f0 4.0f0) var-env func-env)
        "(((1.0f + 2.0f) + 3.0f) + 4.0f)"
        "add float")))


;;;
;;; test COMPILE-FUNCTION function
;;;

(deftest compile-function
  (let ((var-env (empty-variable-environment))
        (func-env (function-environment-add-function 'foo 'int '(int int)
                                                     (empty-function-environment))))
    (is (compile-function '(foo 1 1) var-env func-env)
        "oclcl_tests_lang_compiler_compile_expression_foo(1, 1)"
        "basic case 1")
    (ok (signals (compile-function '(foo 1 1 1) var-env func-env) 'simple-error)))

  (with-empty-env (var-env func-env)
    (is (compile-function '(+ 1 1) var-env func-env) "(1 + 1)"
        "basic case 2")
    (is (compile-function '(- 1) var-env func-env) "-(1)"
        "basic case 3")
    (is (compile-function '(+ (float3 1.0f0 1.0f0 1.0f0) (float3 2.0f0 2.0f0 2.0f0))
                          var-env func-env)
        "((float3)(1.0f, 1.0f, 1.0f) + (float3)(2.0f, 2.0f, 2.0f))"
        "float3 constructor")
    (is (compile-function '(barrier :clk-local-mem-fence) var-env func-env)
        "barrier(CLK_LOCAL_MEM_FENCE)"
        "barrier")))
