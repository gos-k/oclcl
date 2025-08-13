#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.tests.lang.compiler.type-of-expression
  (:use :cl :rove
        :oclcl.tests.utils
        :oclcl.lang.compiler.type-of-expression
        :oclcl.lang.data
        :oclcl.lang.type
        :oclcl.lang.syntax
        :oclcl.lang.environment
        :oclcl.lang.built-in)
  (:import-from :oclcl.lang.compiler.type-of-expression
                :type-of-macro
                :type-of-symbol-macro
                :type-of-literal
                :type-of-reference
                :type-of-inline-if
                :type-of-arithmetic
                :type-of-function)
  (:import-from :arrow-macros
                :->>))

(in-package :oclcl.tests.lang.compiler.type-of-expression)

;;;
;;; test TYPE-OF-EXPRESSION function
;;;

(deftest type-of-expression
  (let ((var-env (empty-variable-environment))
        (func-env (empty-function-environment)))
    (is (type-of-expression 1 var-env func-env) 'int)))

;;;
;;; test TYPE-OF-MACRO function (not implemented)
;;;


;;;
;;; test TYPE-OF-SYMBOL-MACRO function (not implemented)
;;;


;;;
;;; test TYPE-OF-LITERAL function
;;;

(deftest type-of-literal

  (is (type-of-literal t) 'bool
      "basic case 1")

  (is (type-of-literal nil) 'bool
      "basic case 2")

  (is (type-of-literal 1) 'int
      "basic case 3")

  (is (type-of-literal 1.0f0) 'float
      "basic case 4")

  (is (type-of-literal 1.0d0) 'double
      "basic case 5"))


;;;
;;; test TYPE-OF-REFERENCE function
;;;

(deftest type-of-reference-variable
  (let ((var-env (->> (empty-variable-environment)
                   (variable-environment-add-variable 'x 'int)
                   (variable-environment-add-symbol-macro 'y 'y-expansion)
                   (variable-environment-add-variable 'y-expansion 'float)
                   (variable-environment-add-memory 'z 'int 1)))
        (func-env (empty-function-environment)))
    (is (type-of-reference 'x var-env func-env) 'int
        "basic case 1")
    (is (type-of-reference 'z var-env func-env) 'int
        "basic caase 2")
    (ok (signals (type-of-reference 'y var-env func-env) 'simple-error)
        "FORM which is a variable not found.")
    (ok (signals (type-of-reference 'a var-env func-env) 'simple-error)
        "FORM which is a variable not found.")))


(deftest type-of-reference-structure
  (let ((var-env (variable-environment-add-variable 'x 'float3
                                                    (empty-variable-environment)))
        (func-env (empty-function-environment)))
    (is (type-of-reference '(float3-x x) var-env func-env) 'float)
    (is (type-of-reference '(float3-y x) var-env func-env) 'float)
    (ok (signals (type-of-reference '(float4-x x) var-env func-env)
                 'simple-error))))


(deftest type-of-reference-array

  (let ((var-env (variable-environment-add-variable 'x 'int
                                                    (empty-variable-environment)))
        (func-env (empty-function-environment)))
    (ok (signals (type-of-reference '(aref x) var-env func-env) 'simple-error)))

  (let ((var-env (variable-environment-add-variable 'x 'int*
                                                    (empty-variable-environment)))
        (func-env (empty-function-environment)))
    (is (type-of-reference '(aref x 0) var-env func-env) 'int)
    (ok (signals (type-of-reference '(aref x 0 0) var-env func-env)
                 'simple-error)))
  (let ((var-env (variable-environment-add-variable 'x 'int**
                                                    (empty-variable-environment)))
        (func-env (empty-function-environment)))
    (ok (signals (type-of-reference '(aref x 0) var-env func-env) 'simple-error))
    (is (type-of-reference '(aref x 0 0) var-env func-env) 'int)))


;;;
;;; test TYPE-OF-INLINE-IF function
;;;

(deftest type-of-inline-if

  (multiple-value-bind (var-env func-env) (empty-environment)
    (ok (signals (type-of-inline-if '(if) var-env func-env)
                 'simple-error)
        "only if")
    (ok (signals (type-of-inline-if '(if (= 1 1)) var-env func-env)
                 'simple-error)
        "test")
    (ok (signals (type-of-inline-if '(if (= 1 1) 1) var-env func-env)
                 'simple-error)
        "test and then")
    (is (type-of-inline-if '(if (= 1 1) 1 2) var-env func-env)
        'int
        "valid if expression")
    (ok (signals (type-of-inline-if '(if 1 2 3) var-env func-env)
                 'simple-error)
        "test is not bool")
    (ok (signals (type-of-inline-if '(if (= 1 1) 1 2.0) var-env func-env)
                 'simple-error)
        "different type")))


;;;
;;; test TYPE-OF-ARITHMETIC function (not implemented)
;;;


;;;
;;; test TYPE-OF-FUNCTION function
;;;

(deftest type-of-function
  (let ((var-env (empty-variable-environment))
        (func-env (function-environment-add-function 'foo 'int '(int int)
                                                     (empty-function-environment))))
    (is (type-of-function '(+ 1 1) var-env func-env) 'int)
    (is (type-of-function '(foo 1 1) var-env func-env) 'int)
    (is (type-of-function '(+ 1.0f0 1.0f0) var-env func-env) 'float)
    (ok (signals (type-of-function '(+ 1 1.0f0) var-env func-env) 'simple-error))
    (is (type-of-function '(pow 1.0f0 1.0f0) var-env func-env) 'float)
    (is (type-of-function '(half-cos 1.0f0) var-env func-env) 'float)
    (ok (signals (type-of-function '(half-divide 1.0) var-env func-env) 'simple-error))
    (is (type-of-function '(native-cos 1.0f0) var-env func-env) 'float)
    (ok (signals (type-of-function '(native-divide 1.0) var-env func-env) 'simple-error))
    (is (type-of-function '(popcount 1) var-env func-env) 'int)
    (is (type-of-function '(degrees 1.0f0) var-env func-env) 'float)))
