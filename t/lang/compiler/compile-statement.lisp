#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.compiler.compile-statement
  (:use :cl :prove
        :oclcl.lang.util
        :oclcl.lang.data
        :oclcl.lang.type
        :oclcl.lang.syntax
        :oclcl.lang.environment
        :oclcl.lang.compiler.compile-statement)
  (:import-from :oclcl.lang.compiler.compile-statement
                :compile-macro
                :compile-if
                :compile-let
                :compile-symbol-macrolet
                :compile-do
                :compile-with-local-memory
                :compile-set
                :compile-progn
                :compile-return
                :compile-function))
(in-package :oclcl-test.lang.compiler.compile-statement)

(plan nil)


;;;
;;; test COMPILE-STATEMENT function (not implemented)
;;;


;;;
;;; test COMPILE-MACRO function (not implemented)
;;;


;;;
;;; test COMPILE-IF funciton
;;;

(subtest "COMPILE-IF"

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(if t (return) (return)))
          (c-code (unlines "if (true)"
                           "{"
                           "  return;"
                           "}"
                           "else"
                           "{"
                           "  return;"
                           "}")))
      (is (compile-if lisp-code var-env func-env) c-code
          "basic case 1")))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(if t (return 0)))
          (c-code (unlines "if (true)"
                           "{"
                           "  return 0;"
                           "}")))
      (is (compile-if lisp-code var-env func-env) c-code
          "basic case 2")))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(if 1 (return))))
      (is-error (compile-if lisp-code var-env func-env) simple-error))))


;;;
;;; test COMPILE-LET function
;;;

(subtest "COMPILE-LET"

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(let ((i 0))
                       (return)))
          (c-code (unlines "{"
                           "  int i = 0;"
                           "  return;"
                           "}")))
      (is (compile-let lisp-code var-env func-env) c-code
          "basic case 1")))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (is-error (compile-let '(let (i) (return)) var-env func-env)
              simple-error)
    (is-error (compile-let '(let ((i)) (return)) var-env func-env)
              simple-error)
    (is-error (compile-let '(let ((x 1) (y x)) (return y)) var-env func-env)
              simple-error)))


;;;
;;; test COMPILE-SYMBOL-MACROLET function
;;;

(subtest "COMPILE-SYMBOL-MACROLET"

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(symbol-macrolet ((x 1))
                       (return x)))
          (c-code (unlines "return 1;")))
      (is (compile-symbol-macrolet lisp-code var-env func-env) c-code
          "basic case 1"))))



;;;
;;; test COMPILE-DO function
;;;

(subtest "COMPILE-DO"
  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(do ((a 0 (+ a 1))
                           (b 0 (+ b 1)))
                       ((> a 15))
                       (return)))
          (c-code (unlines "for ( int a = 0, int b = 0; ! (a > 15); a = (a + 1), b = (b + 1) )"
                           "{"
                           "  return;"
                           "}")))
      (is (compile-do lisp-code var-env func-env) c-code
          "basic case 1"))))


;;;
;;; test COMPILE-WITH-LOCAL-MEMORY function
;;;

(subtest "COMPILE-WITH-LOCAL-MEMORY"
  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(with-local-memory ((a int 16)
                                           (b float 16 16))
                       (return)))
          (c-code (unlines "{"
                           "  __local int a[16];"
                           "  __local float b[16][16];"
                           "  return;"
                           "}")))
      (is (compile-with-local-memory lisp-code var-env func-env) c-code
          "basic case 1")))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(with-local-memory () (return)))
          (c-code (unlines "{"
                           "  return;"
                           "}")))
      (is (compile-with-local-memory lisp-code var-env func-env) c-code
          "basic case 2")))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(with-local-memory ()))
          (c-code (unlines "{"
                           "}")))
      (is (compile-with-local-memory lisp-code var-env func-env) c-code
          "basic case 3")))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(with-local-memory ((a float))
                       (return a)))
          (c-code (unlines "{"
                           "  __local float a;"
                           "  return a;"
                           "}")))
      (is (compile-with-local-memory lisp-code var-env func-env) c-code
          "basic case 4")))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(with-local-memory ((a float 16 16))
                       (set (aref a 0 0) 1.0)))
          (c-code (unlines "{"
                           "  __local float a[16][16];"
                           "  a[0][0] = 1.0f;"
                           "}")))
      (is (compile-with-local-memory lisp-code var-env func-env) c-code
          "basic case 5")))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(with-local-memory ((a float (+ 16 2)))
                       (set (aref a 0) 1.0)))
          (c-code (unlines "{"
                           "  __local float a[(16 + 2)];"
                           "  a[0] = 1.0f;"
                           "}")))
      (is (compile-with-local-memory lisp-code var-env func-env) c-code
          "basic case 6")))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(with-local-memory (a float)
                       (return))))
      (is-error (compile-with-local-memory lisp-code var-env func-env)
                simple-error)))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(with-local-memory ((a float 16 16))
                       (set (aref a 0) 1.0))))
      (is-error (compile-with-local-memory lisp-code var-env func-env)
                simple-error))))


;;;
;;; test COMPILE-SET function
;;;

(subtest "COMPILE-SET"

  (let ((var-env (variable-environment-add-variable 'x 'int
                                                    (empty-variable-environment)))
        (func-env (empty-function-environment)))
    (is (compile-set '(set x 1) var-env func-env)
        (unlines "x = 1;")
        "basic case 1")
    (is-error (compile-set '(set x 1.0) var-env func-env) simple-error))

  (let ((var-env (variable-environment-add-variable 'x 'int*
                                                    (empty-variable-environment)))
        (func-env (empty-function-environment)))
    (is (compile-set '(set (aref x 0) 1) var-env func-env)
        (unlines "x[0] = 1;")
        "basic case 2")
    (is-error (compile-set '(set (aref x 0) 1.0) var-env func-env)
              simple-error))

  (let ((var-env (variable-environment-add-variable 'x 'float3
                                                    (empty-variable-environment)))
        (func-env (empty-function-environment)))
    (is (compile-set '(set (float3-x x) 1.0) var-env func-env)
        (unlines "x.x = 1.0f;")
        "basic case 3")
    (is-error (compile-set '(set (float3-x x) 1) var-env func-env)
              simple-error)))


;;;
;;; test COMPILE-PROGN function (not implemented)
;;;


;;;
;;; test COMPILE-RETURN function (not implemented)
;;;


;;;
;;; test COMPILE-FUNCTION function (not implemented)
;;;




(finalize)
