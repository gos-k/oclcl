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
                :compile-function
                :compile-sizeof))
(in-package :oclcl.tests.lang.compiler.compile-expression)

;;;
;;; test COMPILE-EXPRESSION function
;;;

(deftest compile-expression
  (with-empty-env (env)
    (is (compile-expression 1 env) "1")))


;;;
;;; test COMPILE-MACRO function
;;;

(deftest compile-macro
  (with-empty-env (env)
    (setf env (function-environment-add-macro 'foo '(x) '(`(+ ,x ,x)) env))
    (is (compile-macro '(foo 1) env) "(1 + 1)"
        "basic case 1")))


;;;
;;; test COMPILE-SYMBOL-MACRO function
;;;

(deftest compile-symbol-macro
  (with-empty-env (env)
    (setf env (variable-environment-add-symbol-macro 'x 1 env))
    (is (compile-symbol-macro 'x env) "1"
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
  (let* ((env (empty-environment))
         (env (variable-environment-add-variable 'x 'int env))
         (env (variable-environment-add-symbol-macro 'y 'y-expansion env))
         (env (variable-environment-add-variable 'y-expansion 'float env))
         (env (variable-environment-add-memory 'z 'int 1 env)))
    (is (compile-reference 'x env) "x"
        "basic case 1")
    (is (compile-reference 'z env)
        "oclcl_tests_lang_compiler_compile_expression_z"
        "basic case 2")
    (ok (signals (compile-reference 'y env) 'simple-error)
        "FORM which is a variable not found.")
    (ok (signals (compile-reference 'a env) 'simple-error)
        "FORM which is a variable not found.")))

(deftest compile-reference-structure
  (let* ((env (empty-environment))
         (env (variable-environment-add-variable 'x 'float3 env)))
    (is (compile-reference '(float3-x x) env) "x.x"
        "basic case 1")
    (is (compile-reference '(float3-y x) env) "x.y"
        "basic case 2")
    (ok (signals (compile-reference '(float4-x x) env)
                 'simple-error))))


(deftest compile-reference-array
  (let* ((env (empty-environment))
         (env (variable-environment-add-variable 'x 'int* env))
         (env (variable-environment-add-variable 'i 'int env)))
    (is (compile-reference '(aref x i) env) "x[i]"
        "basic case 1")))


;;;
;;; test COMPILE-INLINE-IF function
;;;

(deftest compile-inline-if
  (with-empty-env (env)
    (is (compile-inline-if '(if (= 1 1) 1 2) env)
        "((1 == 1) ? 1 : 2)"
        "basic case 1")))


;;;
;;; test COMPILE-ARITHMETIC function
;;;

(deftest compile-arithmetic
  (with-empty-env (env)
    (is (compile-arithmetic '(+ 1 1 1) env) "((1 + 1) + 1)"
        "add integer")
    (is (compile-arithmetic '(- 1 1 1) env) "((1 - 1) - 1)"
        "sub integer")
    (is (compile-arithmetic '(* 1 1 1) env) "((1 * 1) * 1)"
        "mul integer")
    (is (compile-arithmetic '(/ 1 1 1) env) "((1 / 1) / 1)"
        "div integer")
    (is (compile-arithmetic '(mod 1 1 1) env) "((1 % 1) % 1)"
        "mod integer")
    (is (compile-arithmetic '(+ 1 1 (- 1 1)) env) "((1 + 1) + (1 - 1))"
        "mix integer")
    (is (compile-arithmetic '(+ 1.0f0 2.0f0 3.0f0 4.0f0) env)
        "(((1.0f + 2.0f) + 3.0f) + 4.0f)"
        "add float")))


;;;
;;; test COMPILE-FUNCTION function
;;;

(deftest compile-function
  (let* ((env (empty-environment))
         (env (function-environment-add-function 'foo 'int '(int int) env)))
    (is (compile-function '(foo 1 1) env)
        "oclcl_tests_lang_compiler_compile_expression_foo(1, 1)"
        "basic case 1")
    (ok (signals (compile-function '(foo 1 1 1) env) 'simple-error)))

  (with-empty-env (env)
    (is (compile-function '(+ 1 1) env) "(1 + 1)"
        "basic case 2")
    (is (compile-function '(- 1) env) "-(1)"
        "basic case 3")
    (is (compile-function '(+ (float3 1.0f0 1.0f0 1.0f0) (float3 2.0f0 2.0f0 2.0f0))
                          env)
        "((float3)(1.0f, 1.0f, 1.0f) + (float3)(2.0f, 2.0f, 2.0f))"
        "float3 constructor")
    (is (compile-function '(barrier :clk-local-mem-fence) env)
        "barrier(CLK_LOCAL_MEM_FENCE)"
        "barrier")))

;;;
;;; test COMPILE-SIZEOF function
;;;

(deftest compile-sizoef
  (with-empty-env (env)
    (setf env (variable-environment-add-variable 'alfa 'int env))
    (is (compile-sizeof '(sizeof alfa) env) "sizeof(alfa)" "sizeof variable")
    (is (compile-sizeof '(sizeof float) env) "sizeof(float)" "sizeof type")))