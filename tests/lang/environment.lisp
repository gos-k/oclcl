#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.tests.lang.environment
  (:use :cl :rove
        :arrow-macros
        :oclcl.tests.utils
        :oclcl.lang.type
        :oclcl.lang.environment))
(in-package :oclcl.tests.lang.environment)

;;;
;;; test Variable environment - Variable
;;;

(deftest variable-environment-variable
  (let ((env (->> (empty-environment)
                   (variable-environment-add-variable 'x 'int)
                   (variable-environment-add-symbol-macro 'y 1.0))))
    (is (variable-environment-variable-exists-p env 'x) t
        "basic case 1")
    (is (variable-environment-variable-exists-p env 'y) nil
        "basic case 2")
    (is (variable-environment-variable-exists-p env 'z) nil
        "basic case 3")
    (is (variable-environment-variable-name env 'x) 'x
        "basic case 4")
    (is (variable-environment-variable-type env 'x) 'int
        "basic case 5"))

  (let ((env (->> (empty-environment)
                   (variable-environment-add-variable 'x 'int)
                   (variable-environment-add-variable 'x 'float))))
    (is (variable-environment-variable-type env 'x) 'float
        "basic case 6"))

  (let ((env (->> (empty-environment)
                   (variable-environment-add-variable 'x 'int)
                   (variable-environment-add-symbol-macro 'x '1.0))))
    (is (variable-environment-variable-exists-p env 'x) nil
        "basic case 7")
    (is (variable-environment-symbol-macro-exists-p env 'x) t
        "basic case 8")))


;;;
;;; test Variable environment - Symbol macro
;;;

(deftest variable-environment-symbol-macro
  (let ((env (->> (empty-environment)
                   (variable-environment-add-variable 'x 'int)
                   (variable-environment-add-symbol-macro 'y 1.0))))
    (is (variable-environment-symbol-macro-exists-p env 'x) nil
        "basic case 1")
    (is (variable-environment-symbol-macro-exists-p env 'y) t
        "basic case 2")
    (is (variable-environment-symbol-macro-exists-p env 'z) nil
        "basic case 3")
    (is (variable-environment-symbol-macro-name env 'y) 'y
        "basic case 4")
    (is (variable-environment-symbol-macro-expansion env 'y) 1.0
        "basic case 5")))


;;;
;;; test Function environment - Function
;;;

(deftest function-environment-function
  (let ((env (->> (empty-environment)
                    (function-environment-add-function 'foo 'int '(int))
                    (function-environment-add-macro 'bar '(x) '(`(return ,x))))))
    (is (function-environment-function-exists-p env 'foo) t
        "basic case 1")
    (is (function-environment-function-exists-p env 'bar) nil
        "basic case 2")
    (is (function-environment-function-exists-p env 'baz) nil
        "basic case 3")
    (is (function-environment-function-name env 'foo) 'foo
        "basic case 4")
    (is (function-environment-function-c-name env 'foo) "oclcl_tests_lang_environment_foo"
        "basic case 5")
    (is (function-environment-function-return-type env 'foo) 'int
        "basic case 6")
    (is (function-environment-function-argument-types env 'foo) '(int)
        "basic case 7")))


;;; test Variable environment - Memory
;;;

(deftest variable-environment-memory
  (let ((env (variable-environment-add-variable 'z 'int
                                                    (variable-environment-add-memory 'y 'int nil
                                                                                     (variable-environment-add-memory 'x 'int 1
                                                                                                                      (empty-environment))))))
    (is (variable-environment-memory-exists-p env 'x) t
        "basic case 1")
    (is (variable-environment-memory-exists-p env 'y) t
        "basic case 2")
    (is (variable-environment-memory-exists-p env 'z) nil
        "basic case 3")
    (is (variable-environment-memory-name env 'x) 'x
        "basic case 4")
    (is (variable-environment-memory-c-name env 'x)
        "oclcl_tests_lang_environment_x"
        "basic case 5")
    (is (variable-environment-memory-type env 'x) 'int
        "basic case 6")
    (is (variable-environment-memory-expression env 'x) 1
        "basic case 7")
    (is (variable-environment-memory-expression env 'y) nil
        "basic case 8"))

  (ok (signals (variable-environment-add-memory 1 'int 1
                                                (empty-environment))
               'type-error)
      "Invalid name.")

  (ok (signals (variable-environment-add-memory 'x :foo 1
                                                (empty-environment))
               'type-error)
      "Invalid cl-cuda type.")

  (ok (signals (variable-environment-add-memory 'x 'int 1
                                                :foo)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-exists-p :foo 'x)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-exists-p (empty-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (variable-environment-memory-name :foo 'x)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-name (empty-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (variable-environment-memory-c-name :foo 'x)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-c-name (empty-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (variable-environment-memory-type :foo 'x)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-type (empty-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (variable-environment-memory-expression :foo 'x)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-expression (empty-environment)
                                                       1)
               'type-error)
      "Invalid name."))

;;;
;;; test Function environment - Macro
;;;

(deftest function-environment-macro
  (let ((env (->> (empty-environment)
                    (function-environment-add-function 'foo 'int '(int))
                    (function-environment-add-macro 'bar '(x) '(`(return ,x))))))
    (is (function-environment-macro-exists-p env 'foo) nil
        "basic case 1")
    (is (function-environment-macro-exists-p env 'bar) t
        "basic case 2")
    (is (function-environment-macro-exists-p env 'baz) nil
        "basic case 3")
    (is (function-environment-macro-name env 'bar) 'bar
        "basic case 4")
    (is (funcall (function-environment-macro-expander env 'bar) '(1))
        '(return 1)
        "basic case 5")))

;;;
;;; test Define environment - Define
;;;

(deftest define-environment-define
  (let ((env (->> (empty-environment)
                   (define-environment-add-define 'x 1)
                   (define-environment-add-define 'y nil))))
    (ok (define-environment-define-exists-p env 'x))
    (ok (define-environment-define-exists-p env 'y))
    (ng (define-environment-define-exists-p env 'z))
    (is (define-environment-define-name env 'x) 'x)
    (is (define-environment-define-c-name env 'x)
        "oclcl_tests_lang_environment_x")
    (is (define-environment-define-expression env 'x) 1)
    (ng (define-environment-define-expression env 'y)))

  (ok (signals (define-environment-add-define 1 1 (empty-environment))
               'type-error)
      "Invalid name.")

  (ok (signals (define-environment-add-define 'x 1 :foo)
               'type-error)
      "Invalid define environment.")

  (ok (signals (define-environment-define-exists-p :foo 'x)
               'type-error)
      "Invalid define environment.")

  (ok (signals (define-environment-define-exists-p (empty-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (define-environment-define-name :foo 'x)
               'type-error)
      "Invalid define environment.")

  (ok (signals (define-environment-define-name (empty-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (define-environment-define-c-name :foo 'x)
               'type-error)
      "Invalid define environment.")

  (ok (signals (define-environment-define-c-name (empty-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (define-environment-define-expression :foo 'x)
               'type-error)
      "Invalid define environment.")

  (ok (signals (define-environment-define-expression (empty-environment) 1)
               'type-error)
      "Invalid name."))