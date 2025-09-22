#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.tests.lang.compiler.compile-statement
  (:use :cl :rove
        :oclcl.tests.utils
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
                :compile-function)
  (:import-from :serapeum
                :fmt))
(in-package :oclcl.tests.lang.compiler.compile-statement)

(defun %test-compile-statement (statement-func lisp-code c-code message)
  (with-empty-env (var-env func-env)
    (is (apply statement-func (list lisp-code var-env func-env)) c-code
        message)))

;;;
;;; test COMPILE-STATEMENT function
;;;

(deftest compile-statement
  (flet ((test-statement (lisp-code c-code message)
           (%test-compile-statement #'compile-statement lisp-code c-code message)))
    (test-statement '(return) (unlines "return;") "return statement")
    (test-statement '(+ 1 2) (unlines "(1 + 2);") "arithmetic statement")))


;;;
;;; test COMPILE-MACRO function
;;;

(deftest compile-macro
  (with-empty-env (var-env func-env)
    (setf func-env (function-environment-add-macro 'alfa '(x) '(`(let ((,x 0))
                                                                   (* ,x ,x)))
                                                   func-env))
    (flet ((test-statement (lisp-code c-code message)
             (is (compile-macro lisp-code var-env func-env) c-code message)))
      (test-statement '(alfa bravo) (unlines "{"
                                             "  int bravo = 0;"
                                             "  (bravo * bravo);"
                                             "}")
                      "macro statement"))))


;;;
;;; test COMPILE-IF funciton
;;;

(deftest compile-if
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

  (with-empty-env (var-env func-env)
    (let ((lisp-code '(if 1 (return))))
      (ok (signals (compile-if lisp-code var-env func-env) 'simple-error)) )))


;;;
;;; test COMPILE-LET function
;;;

(deftest compile-let
  (defun test-let (lisp-code c-code message)
    (%test-compile-statement #'compile-let lisp-code c-code message))

  (test-let '(let ((i 0))
              (return))
            (unlines "{"
                     "  int i = 0;"
                     "  return;"
                     "}")
            "basic case 1")

  (with-empty-env (var-env func-env)
    (ok (signals (compile-let '(let (i) (return)) var-env func-env)
                 'simple-error))
    (ok (signals (compile-let '(let ((i)) (return)) var-env func-env)
                 'simple-error))
    (ok (signals (compile-let '(let ((x 1) (y x)) (return y)) var-env func-env)
                 'simple-error))))


;;;
;;; test COMPILE-SYMBOL-MACROLET function
;;;

(deftest compile-symbol-macrolet
  (defun test-symbol-macrolet (lisp-code c-code message)
    (%test-compile-statement #'compile-symbol-macrolet lisp-code c-code message))

  (test-symbol-macrolet '(symbol-macrolet ((x 1))
                          (return x))
                        (unlines "return 1;")
                        "basic case 1"))



;;;
;;; test COMPILE-DO function
;;;

(deftest compile-do
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

(deftest compile-with-local-memory
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

  (with-empty-env (var-env func-env)
    (let ((lisp-code '(with-local-memory (a float)
                       (return))))
      (ok (signals (compile-with-local-memory lisp-code var-env func-env)
                   'simple-error))))

  (with-empty-env (var-env func-env)
    (let ((lisp-code '(with-local-memory ((a float 16 16))
                       (set (aref a 0) 1.0f0))))
      (ok (signals (compile-with-local-memory lisp-code var-env func-env)
                   'simple-error)))))


;;;
;;; test COMPILE-SET function
;;;

(deftest compile-set
  (with-empty-env (var-env func-env)
    (setf var-env (variable-environment-add-variable 'x 'int var-env))
    (is (compile-set '(set x 1) var-env func-env)
        (unlines "x = 1;")
        "basic case 1")
    (ok (signals (compile-set '(set x 1.0f0) var-env func-env) 'simple-error)))

  (testing "literal"
    (dolist (type oclcl.lang.type::+scalar-float-types+)
      (testing (fmt "type ~A" type)
        (with-empty-env (var-env func-env)
          (setf var-env (variable-environment-add-variable 'x type var-env))
          (is (compile-set '(set x 1) var-env func-env) (unlines "x = 1;"))))))

  (testing "variable"
    (dolist (type oclcl.lang.type::+scalar-float-types+)
      (dolist (right oclcl.lang.type::+scalar-integer-types+)
        (testing (fmt "type ~A" type)
          (with-empty-env (var-env func-env)
            (setf var-env (variable-environment-add-variable 'x type var-env))
            (testing (fmt "type ~A" right)
              (setf var-env (variable-environment-add-variable 'y type var-env))
              (is (compile-set '(set x y) var-env func-env) (unlines "x = y;"))))))))

  (with-empty-env (var-env func-env)
    (setf var-env (variable-environment-add-variable 'x 'int* var-env))
    (is (compile-set '(set (aref x 0) 1) var-env func-env)
        (unlines "x[0] = 1;")
        "basic case 2")
    (ok (signals (compile-set '(set (aref x 0) 1.0f0) var-env func-env)
                 'simple-error)))
  (with-empty-env (var-env func-env)
    (setf var-env (variable-environment-add-variable 'x 'float3 var-env))
    (is (compile-set '(set (float3-x x) 1.0f0) var-env func-env)
        (unlines "x.x = 1.0f;")
        "basic case 3")
    (ok (compile-set '(set (float3-x x) 1) var-env func-env))))


;;;
;;; test COMPILE-PROGN function
;;;


(deftest compile-progn
  (with-empty-env (var-env func-env)
    (is (compile-progn '(progn) var-env func-env) "")
    (is (compile-progn '(progn (return))  var-env func-env) (unlines "return;"))))

;;;
;;; test COMPILE-RETURN function
;;;

(deftest compile-return
  (with-empty-env (var-env func-env)
    (is (compile-return '(return) var-env func-env) (unlines "return;"))))

;;;
;;; test COMPILE-FUNCTION function
;;;

(deftest compile-function
  (with-empty-env (var-env func-env)
    (setf func-env (function-environment-add-function 'alfa 'int '(int) func-env))
    (is (compile-function '(alfa 1) var-env func-env)
        (unlines "oclcl_tests_lang_compiler_compile_statement_alfa(1);"))))

