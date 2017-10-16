#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.program
  (:use :cl :prove
        :oclcl.lang.program
        :oclcl.lang.type))
(in-package :oclcl-test.lang.program)

(plan nil)


;;;
;;; test MAKE-PROGRAM function
;;;

(subtest "MAKE-PROGRAM"

  (let ((program (make-program)))
    (is (program-function-names program) nil
        "basic case 1")
    (is (program-symbol-macro-names program) nil
        "basic case 2")))


;;; test PROGRAM-MEMORY-NAMES function
;;;

(subtest "PROGRAM-MEMORY-NAMES"
  (let ((program (make-program)))
    (program-define-memory program 'x :global 42)
    (program-define-symbol-macro program 'y 42)
    (is (program-memory-names program) '(x)
        "program basic 1")))
;;;
;;; test PROGRAM-FUNCTION-NAMES function
;;;

(subtest "PROGRAM-FUNCTION-NAMES"

  (let ((program (make-program)))
    (program-define-function program 'foo 'int '((x int)) '((return x)))
    (program-define-macro program 'bar '(x) '(`(return ,x)))
    (is (program-function-names program) '(foo)
        "basic case 1")))


;;;
;;; test PROGRAM-MACRO-NAMES function
;;;

(subtest "PROGRAM-MACRO-NAMES"

  (let ((program (make-program)))
    (program-define-function program 'foo 'int '((x int)) '((return x)))
    (program-define-macro program 'bar '(x) '(`(return ,x)))
    (is (program-macro-names program) '(bar)
        "basic case 1")))


;;;
;;; test PROGRAM-SYMBOL-MACRO-NAMES function
;;;

(subtest "PROGRAM-SYMBOL-MACRO-NAMES"

  (let ((program (make-program)))
    (program-define-symbol-macro program 'x 1.0)
    (is (program-symbol-macro-names program) '(x)
        "program basic 1")))


;;;
;;; test PROGRAM-DEFINE-FUNCTION function
;;;

(subtest "PROGRAM-DEFINE-FUNCTION"

  (let ((program (make-program)))
    (is (program-define-function program 'foo 'int '((x int)) '((return x)))
        'foo "basic case 1"))

  (let ((program (make-program)))
    (is-error (program-define-function program
                                      1 'int '((x int)) '((return x)))
              type-error
              "NAME which is not a oclcl symbol."))

  (let ((program (make-program)))
    (is-error (program-define-function program 'foo 1 '((x int)) '((return x)))
              type-error
              "RETURN-TYPE which is not a oclcl type."))

  (let ((program (make-program)))
    (is-error (program-define-function program 'foo 1 'bar '((return x)))
              type-error
              "ARGUMENTS which are invlalid arguments.")))


;;;
;;; test PROGRAM-FUNCTION-EXISTS-P function
;;;

(subtest "PROGRAM-FUNCTION-EXISTS-P"

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


;;;
;;; test PROGRAM-FUNCTION-C-NAME function
;;;


;;;
;;; test PROGRAM-FUNCTION-RETURN-TYPE function
;;;


;;;
;;; test PROGRAM-FUNCTION-ARGUMENTS function
;;;




;;;
;;; test PROGRAM-FUNCTION-ARGUMENT-VARS function
;;;




;;;
;;; test PROGRAM-FUNCTION-ARGUMENT-TYPES function
;;;




;;;
;;; test PROGRAM-FUNCTION-BODY function
;;;




;;;
;;; test PROGRAM-DEFINE-MACRO function
;;;

(subtest "PROGRAM-DEFINE-MACRO"

  (let ((program (make-program)))
    (is (program-define-macro program 'foo '(x) '(`(return ,x)))
        'foo "basic case 1"))

  (let ((program (make-program)))
    (is-error (program-define-macro program 1 '(x) '(`(return ,x)))
              type-error
              "NAME which is not a oclcl symbol.")))


;;;
;;; test PROGRAM-MACRO-EXISTS-P function
;;;

(subtest "PROGRAM-MACRO-EXISTS-P"

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




;;;
;;; test PROGRAM-MACRO-ARGUMENTS function
;;;




;;;
;;; test PROGRAM-MACRO-BODY function
;;;




;;;
;;; test PROGRAM-MACRO-EXPANDER function
;;;




;;;
;;; test EXPAND-MACRO-1 function
;;;

(subtest "EXPAND-MACRO-1"
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
    (is-error (expand-macro-1 '(foo) program) error)))


;;;
;;; test EXPAND-MACRO function
;;;

(subtest "EXPAND-MACRO"

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
    (is-error (expand-macro '(foo)) error)))


;;;
;;; test PROGRAM-DEFINE-SYMBOL-MACRO function
;;;

(subtest "PROGRAM-DEFINE-SYMBOL-MACRO"

  (let ((program (make-program)))
    (is (program-define-symbol-macro program 'x 1.0)
        'x "basic case 1"))

  (let ((program (make-program)))
    (is-error (program-define-symbol-macro program 1 1.0) type-error
              "NAME which is not a oclcl symbol.")))


;;;
;;; test PROGRAM-SYMBOL-MACRO-EXISTS-P function
;;;

(subtest "PROGRAM-SYMBOL-MACRO-EXISTS-P"

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

(subtest "program-define-memory"
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (is (program-memory-exists-p program 'foo)
        t)
    (is (program-memory-name program 'foo)
        'foo)
    (is (program-memory-c-name program 'foo)
        "oclcl_test_lang_program_foo")
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

  (is-error (program-define-memory :foo 'foo :global 42)
            type-error
            "Invalid program.")

  (let ((program (make-program)))
    (is-error (program-define-memory program "foo" :global 42)
              type-error
              "Invalid name."))

  (let ((program (make-program)))
    (is-error (program-define-memory program 'foo :foo 42)
              type-error
              "Invalid qualifier.")))

(subtest "program-memory-exists-p"
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (program-define-symbol-macro program 'bar 42)
    (is (program-memory-exists-p program 'foo)
        t)
    (is (program-memory-exists-p program 'bar)
        nil)
    (is (program-memory-exists-p program 'baz)
        nil))

  (is-error (program-memory-exists-p :foo 'foo)
            type-error
            "Invalid program.")

  (let ((program (make-program)))
    (is-error (program-memory-exists-p program "foo")
              type-error
              "Invalid name.")))

(subtest "program-memory-name"
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (is (program-memory-name program 'foo)
        'foo))

  (let ((program (make-program)))
    (is-error (program-memory-name program 'foo)
              simple-error
              "Global not found."))

  (is-error (program-memory-name :foo 'foo)
            type-error
            "Invalid program.")

  (let ((program (make-program)))
    (is-error (program-memory-name program "foo")
              type-error
              "Invalid name.")))

(subtest "program-memory-c-name"
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (is (program-memory-c-name program 'foo)
        "oclcl_test_lang_program_foo"))

  (let ((program (make-program)))
    (is-error (program-memory-c-name program 'foo)
              simple-error
              "Global not found."))

  (is-error (program-memory-c-name :foo 'foo)
            type-error
            "Invalid program.")

  (let ((program (make-program)))
    (is-error (program-memory-c-name program "foo")
              type-error
              "Invalid name.")))

(subtest "program-address-space-qualifiers"
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (is (program-address-space-qualifiers program 'foo)
        '(:global)))

  (let ((program (make-program)))
    (is-error (program-address-space-qualifiers program 'foo)
              simple-error
              "Global not found."))

  (is-error (program-address-space-qualifiers :foo 'foo)
            type-error
            "Invalid program.")

  (let ((program (make-program)))
    (is-error (program-address-space-qualifiers program "foo")
              type-error
              "Invalid name.")))

(subtest "program-memory-expression"
  (let ((program (make-program)))
    (program-define-memory program 'foo :global 42)
    (is (program-memory-expression program 'foo)
        42))

  (let ((program (make-program)))
    (is-error (program-memory-expression program 'foo)
              simple-error
              "Global not found."))

  (is-error (program-memory-expression :foo 'foo)
            type-error
            "Invalid program.")

  (let ((program (make-program)))
    (is-error (program-memory-expression program "foo")
              type-error
              "Invalid name.")))

(finalize)
