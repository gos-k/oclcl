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
                :type-of-function
                :type-of-sizeof)
  (:import-from :arrow-macros
                :->>))

(in-package :oclcl.tests.lang.compiler.type-of-expression)

;;;
;;; test TYPE-OF-EXPRESSION function
;;;

(deftest type-of-expression
  (with-empty-env (env)
    (is (type-of-expression 1 env) 'int)))

;;;
;;; test TYPE-OF-MACRO function
;;;

(deftest type-of-macro
  (with-empty-env (env)
    (setf env (function-environment-add-macro 'alfa '(x) '(x) env))
    (is (type-of-macro '(alfa "bravo") env) 'string)))

;;;
;;; test TYPE-OF-SYMBOL-MACRO function
;;;

(deftest type-of-symbol-macro
  (with-empty-env (env)
    (setf env (variable-environment-add-symbol-macro 'alfa "bravo" env))
    (is (type-of-symbol-macro 'alfa env) 'string)))


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
  (with-empty-env (env)
    (setf env (->> env
                    (variable-environment-add-variable 'x 'int)
                    (variable-environment-add-symbol-macro 'y 'y-expansion)
                    (variable-environment-add-variable 'y-expansion 'float)
                    (variable-environment-add-memory 'z 'int 1)))
    (is (type-of-reference 'x env) 'int
        "basic case 1")
    (is (type-of-reference 'z env) 'int
        "basic caase 2")
    (ok (signals (type-of-reference 'y env) 'simple-error)
        "FORM which is a variable not found.")
    (ok (signals (type-of-reference 'a env) 'simple-error)
        "FORM which is a variable not found.")))


(deftest type-of-reference-structure
  (with-empty-env (env)
    (setf env (variable-environment-add-variable 'x 'float3 env))
    (is (type-of-reference '(float3-x x) env) 'float)
    (is (type-of-reference '(float3-y x) env) 'float)
    (ok (signals (type-of-reference '(float4-x x) env)
                 'simple-error))))


(deftest type-of-reference-array
  (with-empty-env (env)
    (setf env (variable-environment-add-variable 'x 'int env))
    (ok (signals (type-of-reference '(aref x) env) 'simple-error)))

  (with-empty-env (env)
    (setf env (variable-environment-add-variable 'x 'int* env))
    (is (type-of-reference '(aref x 0) env) 'int)
    (ok (signals (type-of-reference '(aref x 0 0) env)
                 'simple-error)))

  (with-empty-env (env)
    (setf env (variable-environment-add-variable 'x 'int** env))
    (ok (signals (type-of-reference '(aref x 0) env) 'simple-error))
    (is (type-of-reference '(aref x 0 0) env) 'int)))


;;;
;;; test TYPE-OF-INLINE-IF function
;;;

(deftest type-of-inline-if

  (with-empty-env (env)
    (ok (signals (type-of-inline-if '(if) env)
                 'simple-error)
        "only if")
    (ok (signals (type-of-inline-if '(if (= 1 1)) env)
                 'simple-error)
        "test")
    (ok (signals (type-of-inline-if '(if (= 1 1) 1) env)
                 'simple-error)
        "test and then")
    (is (type-of-inline-if '(if (= 1 1) 1 2) env)
        'int
        "valid if expression")
    (ok (signals (type-of-inline-if '(if 1 2 3) env)
                 'simple-error)
        "test is not bool")
    (ok (signals (type-of-inline-if '(if (= 1 1) 1 2.0) env)
                 'simple-error)
        "different type")))


;;;
;;; test TYPE-OF-ARITHMETIC function
;;;

(deftest type-of-arithmetic
  (with-empty-env (env)
    (ok (type-of-arithmetic '(+ 1 2) env) "arithmetic +")))


;;;
;;; test TYPE-OF-FUNCTION function
;;;

(deftest type-of-function
  (with-empty-env (env)
    (setf env (function-environment-add-function 'foo 'int '(int int) env))
    (is (type-of-function '(+ 1 1) env) 'int)
    (is (type-of-function '(foo 1 1) env) 'int)
    (is (type-of-function '(+ 1.0f0 1.0f0) env) 'float)
    (ok (signals (type-of-function '(+ 1 1.0f0) env) 'simple-error))
    (is (type-of-function '(pow 1.0f0 1.0f0) env) 'float)
    (is (type-of-function '(half-cos 1.0f0) env) 'float)
    (ok (signals (type-of-function '(half-divide 1.0) env) 'simple-error))
    (is (type-of-function '(native-cos 1.0f0) env) 'float)
    (ok (signals (type-of-function '(native-divide 1.0) env) 'simple-error))
    (is (type-of-function '(popcount 1) env) 'int)
    (is (type-of-function '(degrees 1.0f0) env) 'float)))

;;;
;;; test TYPE-OF-SIZEOF function
;;;

(deftest type-of-sizeof
  (with-empty-env (env)
    (setf env (variable-environment-add-variable 'alfa 'int env))
    (is (type-of-sizeof '(sizeof alfa) env) 'size-t)
    (is (type-of-sizeof '(sizeof double) env) 'size-t)))