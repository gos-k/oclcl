#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage :oclcl.tests.lang.program
  (:use :cl :rove
        :oclcl.tests.utils
        :oclcl.lang.program
        :oclcl.lang.type))
(in-package :oclcl.tests.lang.program)

;;;
;;; test MAKE-PROGRAM function
;;;

(deftest make-program

  (let ((program (make-program)))
    (is (program-function-names program) nil
        "basic case 1")
    (is (program-symbol-macro-names program) nil
        "basic case 2")))


;;; test PROGRAM-MEMORY-NAMES function
;;;

(deftest program-memory-names
  (let ((program (make-program)))
    (program-define-memory program 'x :global 42)
    (program-define-symbol-macro program 'y 42)
    (is (program-memory-names program) '(x)
        "program basic 1")))
;;;
;;; test PROGRAM-FUNCTION-NAMES function
;;;

(deftest program-function-names

  (let ((program (make-program)))
    (program-define-function program 'foo 'int '((x int)) '((return x)))
    (program-define-macro program 'bar '(x) '(`(return ,x)))
    (is (program-function-names program) '(foo)
        "basic case 1")))


;;;
;;; test PROGRAM-MACRO-NAMES function
;;;

(deftest program-macro-names

  (let ((program (make-program)))
    (program-define-function program 'foo 'int '((x int)) '((return x)))
    (program-define-macro program 'bar '(x) '(`(return ,x)))
    (is (program-macro-names program) '(bar)
        "basic case 1")))


;;;
;;; test PROGRAM-SYMBOL-MACRO-NAMES function
;;;

(deftest program-symbol-macro-names

  (let ((program (make-program)))
    (program-define-symbol-macro program 'x 1.0)
    (is (program-symbol-macro-names program) '(x)
        "program basic 1")))


;;;
;;; test PROGRAM-DEFINE-FUNCTION function
;;;

(deftest program-define-function

  (let ((program (make-program)))
    (is (program-define-function program 'foo 'int '((x int)) '((return x)))
        'foo "basic case 1"))

  (let ((program (make-program)))
    (ok (signals (program-define-function program
                                          1 'int '((x int)) '((return x)))
                 'type-error)
        "NAME which is not a oclcl symbol."))

  (let ((program (make-program)))
    (ok (signals (program-define-function program 'foo 1 '((x int)) '((return x)))
                 'type-error)
        "RETURN-TYPE which is not a oclcl type."))

  (let ((program (make-program)))
    (ok (signals (program-define-function program 'foo 1 'bar '((return x)))
                 'type-error)
        "ARGUMENTS which are invlalid arguments.")))


;;;
;;; test PROGRAM-FUNCTION-EXISTS-P function
;;;

(deftest program-function-exists-p

  (let ((program (make-program)))
    (program-define-function program 'foo 'int '((x int)) '((return x)))
    (program-define-macro program 'bar '(x) '(`(return ,x)))
    (is (program-function-exists-p program 'foo) t
        "basic case 1")
    (is (program-function-exists-p program 'bar) nil
        "basic case 2")
    (is (program-function-exists-p program 'baz) nil
        "basic case 3")))


;;;
;;; test PROGRAM-FUNCTION-NAME function
;;;

(deftest program-function-name
  (let ((program (make-program)))
    (program-define-function program 'alfa 'int '((x bool)) '((return x)))
    (is (program-function-name program 'alfa) 'alfa)))

;;;
;;; test PROGRAM-FUNCTION-C-NAME function
;;;

(deftest program-function-c-name
  (let ((program (make-program)))
    (program-define-function program 'alfa 'int '((x bool)) '((return x)))
    (is (program-function-c-name program 'alfa) "oclcl_tests_lang_program_alfa")))

;;;
;;; test PROGRAM-FUNCTION-RETURN-TYPE function
;;;

(deftest program-function-return-type
  (let ((program (make-program)))
    (program-define-function program 'alfa 'int '((x bool)) '((return x)))
    (is (program-function-return-type program 'alfa) 'int)))

;;;
;;; test PROGRAM-FUNCTION-ARGUMENTS function
;;;

(deftest program-function-arguments
  (let ((program (make-program)))
    (program-define-function program 'alfa 'int '((x bool)) '((return x)))
    (is (program-function-arguments program 'alfa) '((x bool)))))

;;;
;;; test PROGRAM-FUNCTION-ARGUMENT-VARS function
;;;

(deftest program-function-argument-vars
  (let ((program (make-program)))
    (program-define-function program 'alfa 'int '((x bool)) '((return x)))
    (is (program-function-argument-vars program 'alfa) '(x))))

;;;
;;; test PROGRAM-FUNCTION-ARGUMENT-TYPES function
;;;

(deftest program-function-argument-types
  (let ((program (make-program)))
    (program-define-function program 'alfa 'int '((x bool)) '((return x)))
    (is (program-function-argument-types program 'alfa) '(bool))))

;;;
;;; test PROGRAM-FUNCTION-BODY function
;;;

(deftest program-function-argument-body
  (let ((program (make-program)))
    (program-define-function program 'alfa 'int '((x bool)) '((return x)))
    (is (program-function-body program 'alfa) '((return x)))))

;;;
;;; test PROGRAM-DEFINE-MACRO function
;;;

(deftest program-define-macro

  (let ((program (make-program)))
    (is (program-define-macro program 'foo '(x) '(`(return ,x)))
        'foo "basic case 1"))

  (let ((program (make-program)))
    (ok (signals (program-define-macro program 1 '(x) '(`(return ,x)))
                 'type-error)
        "NAME which is not a oclcl symbol.")))


;;;
;;; test PROGRAM-MACRO-EXISTS-P function
;;;

(deftest program-macro-exists-p

  (let ((program (make-program)))
    (program-define-function program 'foo 'int '((x int)) '((return x)))
    (program-define-macro program 'bar '(x) '(`(return ,x)))
    (is (program-macro-exists-p program 'foo) nil
        "basic case 1")
    (is (program-macro-exists-p program 'bar) t
        "basic case 2")
    (is (program-macro-exists-p program 'baz) nil
        "basic case 3")))


;;;
;;; test PROGRAM-MACRO-NAME function
;;;


(deftest program-macro-name
  (let ((program (make-program)))
    (program-define-macro program 'bravo '(x) '(`(return ,x)))
    (is (program-macro-name program 'bravo) 'bravo)))

;;;
;;; test PROGRAM-MACRO-ARGUMENTS function
;;;

(deftest program-macro-arguments
  (let ((program (make-program)))
    (program-define-macro program 'bravo '(x) '(`(return ,x)))
    (is (program-macro-arguments program 'bravo) '(x))))

;;;
;;; test PROGRAM-MACRO-BODY function
;;;

(deftest program-macro-body
  (let ((program (make-program)))
    (program-define-macro program 'bravo '(x) '(`(return ,x)))
    (is (program-macro-body program 'bravo) '(`(return ,x)))))

;;;
;;; test PROGRAM-MACRO-EXPANDER function
;;;

(deftest program-macro-body
  (let ((program (make-program)))
    (program-define-macro program 'bravo '(x) '(`(return ,x)))
    (ok (equalp (program-macro-body program 'bravo) '(`(return ,x))))))

;;;
;;; test EXPAND-MACRO-1 function
;;;

(deftest expand-macro-1
  (let ((program (make-program)))
    (program-define-macro program 'foo '(x) '(`(return ,x)))
    (program-define-macro program 'bar '(x) '(`(foo ,x)))
    (program-define-symbol-macro program 'a 1.0)
    (program-define-symbol-macro program 'b 'a)
    (is-values (expand-macro-1 '(foo 1) program) '((return 1) t))
    (is-values (expand-macro-1 '(bar 1) program) '((foo 1) t))
    (is-values (expand-macro-1 '(baz 1) program) '((baz 1) nil))
    (is-values (expand-macro-1 'a program) '(1.0 t))
    (is-values (expand-macro-1 'b program) '(a t))
    (is-values (expand-macro-1 'c program) '(c nil))
    (ok (signals (expand-macro-1 '(foo) program) 'error))))


;;;
;;; test EXPAND-MACRO function
;;;

(deftest expand-macro

  (let ((program (make-program)))
    (program-define-macro program 'foo '(x) '(`(return ,x)))
    (program-define-macro program 'bar '(x) '(`(foo ,x)))
    (program-define-symbol-macro program 'a 1.0)
    (program-define-symbol-macro program 'b 'a)
    (is-values (expand-macro '(foo 1) program) '((return 1) t))
    (is-values (expand-macro '(bar 1) program) '((return 1) t))
    (is-values (expand-macro '(baz 1) program) '((baz 1) nil))
    (is-values (expand-macro 'a program) '(1.0 t))
    (is-values (expand-macro 'b program) '(1.0 t))
    (is-values (expand-macro 'c program) '(c nil))
    (ok (signals (expand-macro '(foo) program) 'error))))


;;;
;;; test PROGRAM-DEFINE-SYMBOL-MACRO function
;;;

(deftest program-define-symbol-macro

  (let ((program (make-program)))
    (is (program-define-symbol-macro program 'x 1.0)
        'x "basic case 1"))

  (let ((program (make-program)))
    (ok (signals (program-define-symbol-macro program 1 1.0) 'type-error)
        "NAME which is not a oclcl symbol.")))


;;;
;;; test PROGRAM-SYMBOL-MACRO-EXISTS-P function
;;;

(deftest program-symbol-macro-exists-p

  (let ((program (make-program)))
    (program-define-symbol-macro program 'x 1.0)
    (is (program-symbol-macro-exists-p program 'x) t
        "basic case 1")
    (is (program-symbol-macro-exists-p program 'y) nil
        "basic case 2")))


;;;
;;; test PROGRAM-SYMBOL-MACRO-NAME function
;;;




;;;
;;; test PROGRAM-SYMBOL-MACRO-EXPANSION function
;;;

;;; Global
;;;

(deftest program-define-memory
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (is (program-memory-exists-p program 'foo)
        t)
    (is (program-memory-name program 'foo)
        'foo)
    (is (program-memory-c-name program 'foo)
        "oclcl_tests_lang_program_foo")
    (is (program-address-space-qualifiers program 'foo)
        '(:global))
    (is (program-memory-expression program 'foo)
        42))

  (let ((program (make-program)))
    ;; Name in variable namespace should be overwrited.
    (program-define-symbol-macro program 'foo 42)
    (program-define-memory program 'foo :global 42)
    (is (program-memory-exists-p program 'foo)
        t)
    (is (program-symbol-macro-exists-p program 'foo)
        nil))

  (let ((program (make-program)))
    ;; Give multiple qualifiers.
    (program-define-memory program 'foo '(:global :constant) 42)
    (is (program-address-space-qualifiers program 'foo)
        '(:global :constant)))

  (ok (signals (program-define-memory :foo 'foo :global 42)
               'type-error)
      "Invalid program.")

  (let ((program (make-program)))
    (ok (signals (program-define-memory program "foo" :global 42)
                 'type-error)
        "Invalid name."))

  (let ((program (make-program)))
    (ok (signals (program-define-memory program 'foo :foo 42)
                 'type-error)
        "Invalid qualifier.")))

(deftest program-memory-exists-p
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (program-define-symbol-macro program 'bar 42)
    (is (program-memory-exists-p program 'foo)
        t)
    (is (program-memory-exists-p program 'bar)
        nil)
    (is (program-memory-exists-p program 'baz)
        nil))

  (ok (signals (program-memory-exists-p :foo 'foo)
               'type-error)
      "Invalid program.")

  (let ((program (make-program)))
    (ok (signals (program-memory-exists-p program "foo")
                 'type-error)
        "Invalid name.")))

(deftest program-memory-name
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (is (program-memory-name program 'foo)
        'foo))

  (let ((program (make-program)))
    (ok (signals (program-memory-name program 'foo)
                 'undefined-program-variable)
        "Global memory should not be found."))

  (ok (signals (program-memory-name :foo 'foo)
               'type-error)
      "Invalid program.")

  (let ((program (make-program)))
    (ok (signals (program-memory-name program "foo")
                 'type-error)
        "Invalid name.")))

(deftest program-memory-c-name
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (is (program-memory-c-name program 'foo)
        "oclcl_tests_lang_program_foo"))

  (let ((program (make-program)))
    (ok (signals (program-memory-c-name program 'foo)
                 'undefined-program-variable)
        "Global memory should not be found.."))

  (ok (signals (program-memory-c-name :foo 'foo)
               'type-error)
      "Invalid program.")

  (let ((program (make-program)))
    (ok (signals (program-memory-c-name program "foo")
                 'type-error)
        "Invalid name.")))

(deftest program-address-space-qualifiers
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (is (program-address-space-qualifiers program 'foo)
        '(:global)))

  (let ((program (make-program)))
    (ok (signals (program-address-space-qualifiers program 'foo)
                 'undefined-program-variable)
        "Global memory should not be found.."))

  (ok (signals (program-address-space-qualifiers :foo 'foo)
               'type-error)
      "Invalid program.")

  (let ((program (make-program)))
    (ok (signals (program-address-space-qualifiers program "foo")
                 'type-error)
        "Invalid name.")))

(deftest program-memory-expression
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (is (program-memory-expression program 'foo)
        42))

  (let ((program (make-program)))
    (ok (signals (program-memory-expression program 'foo)
                 'undefined-program-variable)
        "Global memory should not be found.."))

  (ok (signals (program-memory-expression :foo 'foo)
               'type-error)
      "Invalid program.")

  (let ((program (make-program)))
    (ok (signals (program-memory-expression program "foo")
                 'type-error)
        "Invalid name.")))
