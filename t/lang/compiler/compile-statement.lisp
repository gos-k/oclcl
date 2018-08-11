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

(defun %test-compile-statement (statement-func lisp-code c-code message)
  (multiple-value-bind (var-env func-env) (empty-environment)
    (is (apply statement-func (list lisp-code var-env func-env)) c-code
        message)))

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
  (defun test-if (lisp-code c-code message)
    (%test-compile-statement #'compile-if lisp-code c-code message))

  (test-if '(if t (return) (return))
           (unlines "if (true)"
                    "{"
                    "  return;"
                    "}"
                    "else"
                    "{"
                    "  return;"
                    "}")
           "test if else")

  (test-if '(if t (return 0))
           (unlines "if (true)"
                    "{"
                    "  return 0;"
                    "}")
           "test if")

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(if 1 (return))))
      (is-error (compile-if lisp-code var-env func-env) simple-error))))


;;;
;;; test COMPILE-LET function
;;;

(subtest "COMPILE-LET"
  (defun test-let (lisp-code c-code message)
    (%test-compile-statement #'compile-let lisp-code c-code message))

  (test-let '(let ((i 0))
              (return))
            (unlines "{"
                     "  int i = 0;"
                     "  return;"
                     "}")
            "basic case 1")

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
  (defun test-symbol-macrolet (lisp-code c-code message)
    (%test-compile-statement #'compile-symbol-macrolet lisp-code c-code message))

  (test-symbol-macrolet '(symbol-macrolet ((x 1))
                          (return x))
                        (unlines "return 1;")
                        "basic case 1"))



;;;
;;; test COMPILE-DO function
;;;

(subtest "COMPILE-DO"
  (defun test-do (lisp-code c-code message)
    (%test-compile-statement #'compile-do lisp-code c-code message))

  (test-do '(do ((a 0 (+ a 1))
                   (b 0 (+ b 1)))
               ((> a 15))
             (return))
           (unlines "for ( int a = 0, int b = 0; ! (a > 15); a = (a + 1), b = (b + 1) )"
                    "{"
                    "  return;"
                    "}")
           "basic case 1"))


;;;
;;; test COMPILE-WITH-LOCAL-MEMORY function
;;;

(subtest "COMPILE-WITH-LOCAL-MEMORY"
  (defun test-local-memory (lisp-code c-code message)
    (%test-compile-statement #'compile-with-local-memory lisp-code c-code message))

  (test-local-memory '(with-local-memory ((a int 16)
                                          (b float 16 16))
                       (return))
                     (unlines "{"
                              "  __local int a[16];"
                              "  __local float b[16][16];"
                              "  return;"
                              "}")
                     "basic case 1")

  (test-local-memory '(with-local-memory () (return))
                     (unlines "{"
                              "  return;"
                              "}")
                     "basic case 2")

  (test-local-memory '(with-local-memory ())
                     (unlines "{"
                              "}")
                     "basic case 3")

  (test-local-memory '(with-local-memory ((a float))
                       (return a))
                     (unlines "{"
                              "  __local float a;"
                              "  return a;"
                              "}")
                     "basic case 4")

  (test-local-memory '(with-local-memory ((a float 16 16))
                       (set (aref a 0 0) 1.0f0))
                     (unlines "{"
                              "  __local float a[16][16];"
                              "  a[0][0] = 1.0f;"
                              "}")
                     "basic case 5")

  (test-local-memory '(with-local-memory ((a float (+ 16 2)))
                       (set (aref a 0) 1.0f0))
                     (unlines "{"
                              "  __local float a[(16 + 2)];"
                              "  a[0] = 1.0f;"
                              "}")
                     "store to local memory")

  (test-local-memory '(with-local-memory ((a float (+ 16 2)))
                       (let ((b 0.0f0))
                         (set b (aref a 0))))
                     (unlines "{"
                              "  __local float a[(16 + 2)];"
                              "  {"
                              "    float b = 0.0f;"
                              "    b = a[0];"
                              "  }"
                              "}")
                     "load from local memory")

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(with-local-memory (a float)
                       (return))))
      (is-error (compile-with-local-memory lisp-code var-env func-env)
                simple-error)))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (let ((lisp-code '(with-local-memory ((a float 16 16))
                       (set (aref a 0) 1.0f0))))
      (is-error (compile-with-local-memory lisp-code var-env func-env)
                simple-error))))


;;;
;;; test COMPILE-SET function
;;;

(subtest "COMPILE-SET"

  (multiple-value-bind (var-env func-env) (empty-environment)
    (setf var-env (variable-environment-add-variable 'x 'int var-env))
    (is (compile-set '(set x 1) var-env func-env)
        (unlines "x = 1;")
        "basic case 1")
    (is-error (compile-set '(set x 1.0f0) var-env func-env) simple-error))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (setf var-env (variable-environment-add-variable 'x 'int* var-env))
    (is (compile-set '(set (aref x 0) 1) var-env func-env)
        (unlines "x[0] = 1;")
        "basic case 2")
    (is-error (compile-set '(set (aref x 0) 1.0f0) var-env func-env)
              simple-error))

  (multiple-value-bind (var-env func-env) (empty-environment)
    (setf var-env (variable-environment-add-variable 'x 'float3 var-env))
    (is (compile-set '(set (float3-x x) 1.0f0) var-env func-env)
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
