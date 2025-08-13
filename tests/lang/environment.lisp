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
  (let ((var-env (->> (empty-variable-environment)
                   (variable-environment-add-variable 'x 'int)
                   (variable-environment-add-symbol-macro 'y 1.0))))
    (is (variable-environment-variable-exists-p var-env 'x) t
        "basic case 1")
    (is (variable-environment-variable-exists-p var-env 'y) nil
        "basic case 2")
    (is (variable-environment-variable-exists-p var-env 'z) nil
        "basic case 3")
    (is (variable-environment-variable-name var-env 'x) 'x
        "basic case 4")
    (is (variable-environment-variable-type var-env 'x) 'int
        "basic case 5"))

  (let ((var-env (->> (empty-variable-environment)
                   (variable-environment-add-variable 'x 'int)
                   (variable-environment-add-variable 'x 'float))))
    (is (variable-environment-variable-type var-env 'x) 'float
        "basic case 6"))

  (let ((var-env (->> (empty-variable-environment)
                   (variable-environment-add-variable 'x 'int)
                   (variable-environment-add-symbol-macro 'x '1.0))))
    (is (variable-environment-variable-exists-p var-env 'x) nil
        "basic case 7")
    (is (variable-environment-symbol-macro-exists-p var-env 'x) t
        "basic case 8")))


;;;
;;; test Variable environment - Symbol macro
;;;

(deftest variable-environment-symbol-macro
  (let ((var-env (->> (empty-variable-environment)
                      (variable-environment-add-variable 'x 'int)
                      (variable-environment-add-symbol-macro 'y 1.0))))
    (is (variable-environment-symbol-macro-exists-p var-env 'x) nil
        "basic case 1")
    (is (variable-environment-symbol-macro-exists-p var-env 'y) t
        "basic case 2")
    (is (variable-environment-symbol-macro-exists-p var-env 'z) nil
        "basic case 3")
    (is (variable-environment-symbol-macro-name var-env 'y) 'y
        "basic case 4")
    (is (variable-environment-symbol-macro-expansion var-env 'y) 1.0
        "basic case 5")))


;;;
;;; test Function environment - Function
;;;

(deftest function-environment-function
  (let ((func-env (->> (empty-function-environment)
                       (function-environment-add-function 'foo 'int '(int))
                       (function-environment-add-macro 'bar '(x) '(`(return ,x))))))
    (is (function-environment-function-exists-p func-env 'foo) t
        "basic case 1")
    (is (function-environment-function-exists-p func-env 'bar) nil
        "basic case 2")
    (is (function-environment-function-exists-p func-env 'baz) nil
        "basic case 3")
    (is (function-environment-function-name func-env 'foo) 'foo
        "basic case 4")
    (is (function-environment-function-c-name func-env 'foo) "oclcl_tests_lang_environment_foo"
        "basic case 5")
    (is (function-environment-function-return-type func-env 'foo) 'int
        "basic case 6")
    (is (function-environment-function-argument-types func-env 'foo) '(int)
        "basic case 7")))


;;; test Variable environment - Memory
;;;

(deftest variable-environment-memory
  (let ((var-env (variable-environment-add-variable 'z 'int
                                                    (variable-environment-add-memory 'y 'int nil
                                                                                     (variable-environment-add-memory 'x 'int 1
                                                                                                                      (empty-variable-environment))))))
    (is (variable-environment-memory-exists-p var-env 'x) t
        "basic case 1")
    (is (variable-environment-memory-exists-p var-env 'y) t
        "basic case 2")
    (is (variable-environment-memory-exists-p var-env 'z) nil
        "basic case 3")
    (is (variable-environment-memory-name var-env 'x) 'x
        "basic case 4")
    (is (variable-environment-memory-c-name var-env 'x)
        "oclcl_tests_lang_environment_x"
        "basic case 5")
    (is (variable-environment-memory-type var-env 'x) 'int
        "basic case 6")
    (is (variable-environment-memory-expression var-env 'x) 1
        "basic case 7")
    (is (variable-environment-memory-expression var-env 'y) nil
        "basic case 8"))

  (ok (signals (variable-environment-add-memory 1 'int 1
                                                (empty-variable-environment))
               'type-error)
      "Invalid name.")

  (ok (signals (variable-environment-add-memory 'x :foo 1
                                                (empty-variable-environment))
               'type-error)
      "Invalid cl-cuda type.")

  (ok (signals (variable-environment-add-memory 'x 'int 1
                                                :foo)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-exists-p :foo 'x)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-exists-p (empty-variable-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (variable-environment-memory-name :foo 'x)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-name (empty-variable-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (variable-environment-memory-c-name :foo 'x)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-c-name (empty-variable-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (variable-environment-memory-type :foo 'x)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-type (empty-variable-environment) 1)
               'type-error)
      "Invalid name.")

  (ok (signals (variable-environment-memory-expression :foo 'x)
               'type-error)
      "Invalid variable environment.")

  (ok (signals (variable-environment-memory-expression (empty-variable-environment)
                                                       1)
               'type-error)
      "Invalid name."))

;;;
;;; test Function environment - Macro
;;;

(deftest function-environment-macro
  (let ((func-env (->> (empty-function-environment)
                    (function-environment-add-function 'foo 'int '(int))
                    (function-environment-add-macro 'bar '(x) '(`(return ,x))))))
    (is (function-environment-macro-exists-p func-env 'foo) nil
        "basic case 1")
    (is (function-environment-macro-exists-p func-env 'bar) t
        "basic case 2")
    (is (function-environment-macro-exists-p func-env 'baz) nil
        "basic case 3")
    (is (function-environment-macro-name func-env 'bar) 'bar
        "basic case 4")
    (is (funcall (function-environment-macro-expander func-env 'bar) '(1))
        '(return 1)
        "basic case 5")))
