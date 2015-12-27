#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.compiler.compile-expression
  (:use :cl :prove
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
                :compile-reference
                :compile-inline-if
                :compile-arithmetic
                :compile-function))
(in-package :oclcl-test.lang.compiler.compile-expression)

(plan nil)


;;;
;;; test COMPILE-EXPRESSION function
;;;

(subtest "COMPILE-EXPRESSION"
  (multiple-value-bind (var-env func-env) (empty-environment)
    (is (compile-expression 1 var-env func-env) "1")))


;;;
;;; test COMPILE-MACRO function
;;;

(subtest "COMPILE-MACRO"
  (let ((var-env (empty-variable-environment))
        (func-env (function-environment-add-macro 'foo '(x) '(`(+ ,x ,x))
                                                  (empty-function-environment))))
    (is (compile-macro '(foo 1) var-env func-env) "(1 + 1)"
        "basic case 1")))


;;;
;;; test COMPILE-SYMBOL-MACRO function
;;;

(subtest "COMPILE-SYMBOL-MACRO"
  (let ((var-env (variable-environment-add-symbol-macro 'x 1
                                                        (empty-variable-environment)))
        (func-env (empty-function-environment)))
    (is (compile-symbol-macro 'x var-env func-env) "1"
        "basic case 1")))


;;;
;;; test COMPILE-LITERAL function
;;;

(subtest "COMPILE-LITERAL"

  (is (compile-literal t) "true"
      "basic case 1")

  (is (compile-literal nil) "false"
      "basic case 2")

  (is (compile-literal 1) "1"
      "basic case 3")

  (is (compile-literal 1.0) "1.0f"
      "basic case 4")

  (is (compile-literal 1.0d0) "1.0"
      "basic case 5"))


;;;
;;; test COMPILE-REFERENCE funcion
;;;

(subtest "COMPILE-REFERENCE - VARIABLE"
  (let* ((add-var (variable-environment-add-variable 'x 'int (empty-variable-environment)))
         (add-symbol-macro (variable-environment-add-symbol-macro 'y 'y-expansion add-var))
         (var-env (variable-environment-add-variable 'y-expansion 'float add-symbol-macro))
         (func-env (empty-function-environment)))
    (is (compile-reference 'x var-env func-env) "x"
        "basic case 1")
    (is-error (compile-reference 'y var-env func-env) simple-error
              "FORM which is a variable not found.")
    (is-error (compile-reference 'a var-env func-env) simple-error
              "FORM which is a variable not found.")))

(subtest "COMPILE-REFERENCE - STRUCTURE"
  (let ((var-env (variable-environment-add-variable 'x 'float3
                                                    (empty-variable-environment)))
        (func-env (empty-function-environment)))
    (is (compile-reference '(float3-x x) var-env func-env) "x.x"
        "basic case 1")
    (is (compile-reference '(float3-y x) var-env func-env) "x.y"
        "basic case 2")
    (is-error (compile-reference '(float4-x x) var-env func-env)
              simple-error)))


(subtest "COMPILE-REFERENCE - ARRAY"
  (let* ((add-var (variable-environment-add-variable 'x 'int* (empty-variable-environment)))
         (var-env (variable-environment-add-variable 'i 'int add-var))
         (func-env (empty-function-environment)))
    (is (compile-reference '(aref x i) var-env func-env) "x[i]"
        "basic case 1")))


;;;
;;; test COMPILE-INLINE-IF function
;;;

(subtest "COMPILE-INLINE-IF"
  (multiple-value-bind (var-env func-env) (empty-environment)
    (is (compile-inline-if '(if (= 1 1) 1 2) var-env func-env)
        "((1 == 1) ? 1 : 2)"
        "basic case 1")))


;;;
;;; test COMPILE-ARITHMETIC function
;;;

(subtest "COMPILE-ARITHMETIC"
  (multiple-value-bind (var-env func-env) (empty-environment)
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
    (is (compile-arithmetic '(+ 1.0 2.0 3.0 4.0) var-env func-env)
        "(((1.0f + 2.0f) + 3.0f) + 4.0f)"
        "add float")))


;;;
;;; test COMPILE-FUNCTION function
;;;

(subtest "COMPILE-FUNCTION"
  (let ((var-env (empty-variable-environment))
        (func-env (function-environment-add-function 'foo 'int '(int int)
                                                     (empty-function-environment))))
    (is (compile-function '(foo 1 1) var-env func-env)
        "oclcl_test_lang_compiler_compile_expression_foo( 1, 1 )"
        "basic case 1")
    (is-error (compile-function '(foo 1 1 1) var-env func-env) simple-error))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (is (compile-function '(+ 1 1) var-env func-env) "(1 + 1)"
        "basic case 2"))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (is (compile-function '(- 1) var-env func-env) "-( 1 )"
        "basic case 3")
    (is (compile-function '(+ (float3 1.0 1.0 1.0) (float3 2.0 2.0 2.0))
                          var-env func-env)
        "(make_float3( 1.0f, 1.0f, 1.0f ) + make_float3( 2.0f, 2.0f, 2.0f ))"
        "basic case 4")))

(multiple-value-bind (var-env func-env) (empty-environment)
  (is (compile-function '(syncthreads) var-env func-env) "__syncthreads()"
      "basic case 1"))


(finalize)
