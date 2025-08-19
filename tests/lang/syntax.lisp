#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.tests.lang.syntax
  (:use :cl :rove
        :oclcl.tests.utils
        :oclcl.lang.data
        :oclcl.lang.syntax))
(in-package :oclcl.tests.lang.syntax)

;;;
;;; test Symbol macro
;;;

(deftest symbol-macro
  (is (symbol-macro-p 'alfa) t))

;;;
;;; test Macro
;;;

(deftest macro

  (is (macro-p '(+ 1 1)) t
      "basic case 1")
  (is (macro-p '(foo 1)) t
      "basic case 2")
  (is (macro-p 'bar) nil
      "basic case 3")

  (is (macro-operator '(+ 1 1)) '+
      "basic case 4")

  (is (macro-operands '(+ 1 1)) '(1 1)
      "basic case 5"))


;;;
;;; test Literal
;;;

(deftest literal

  (is (literal-p 't) t
      "basic case 1")
  (is (literal-p 'nil) t
      "basic case 2")
  (is (literal-p 1) t
      "basic case 3")
  (is (literal-p 1.0) t
      "basic case 4")
  (is (literal-p 1.0d0) t
      "basic case 5")
  (is (literal-p "literal") t
      "string literal"))

;;; test OpenCL literal
;;;

(deftest opencl-literal
  (is (opencl-literal-p :clk-global-mem-fence) t))


;;;
;;; test Reference
;;;

(deftest reference

  (is (reference-p 'x) t
      "basic case 1")
  (is (reference-p '(float3-x x)) t
      "basic case 2")
  (is (reference-p '(float4-w x)) t
      "basic case 3")
  (is (reference-p '(aref x)) t
      "basic case 4")
  (is (reference-p '(aref x i)) t
      "basic case 5")
  (is (reference-p '(aref x i i)) t
      "basic case 6"))


;;;
;;; test Inline-if
;;;

(deftest inline-if

  (is (inline-if-p '(if)) t
      "basic case 1")
  (is (inline-if-p '(if t)) t
      "basic case 2")
  (is (inline-if-p '(if t 2)) t
      "basic case 3")
  (is (inline-if-p '(if t 2 3)) t
      "basic case 4")
  (is (inline-if-p '(if t 2 3 4)) t
      "basic case 5"))


;;;
;;; test Arithmetic
;;;

(deftest arithmetic
  (is (arithmetic-p '(+ 1)) t)
  (is (arithmetic-operator '(+ 1)) '+)
  (is (arithmetic-operands '(* 1 2)) '(1 2)))

;;;
;;; test Function application
;;;

(deftest function-application

  (is (function-p 'a) nil
      "basic case 1")
  (is (function-p '()) nil
      "basic case 2")
  (is (function-p '1) nil
      "basic case 3")
  (is (function-p '(foo)) t
      "basic case 4")
  (is (function-p '(+ 1 1)) t
      "basic case 5")
  (is (function-p '(foo 1 1)) t
      "basic case 6")

  (ok (signals (function-operator 'a) 'simple-error)
      "FORM which is an invalid function application.")
  (is (function-operator '(foo)) 'foo
      "basic case 7")
  (is (function-operator '(+ 1 1)) '+
      "basic case 8")
  (is (function-operator '(foo 1 1)) 'foo
      "basic case 9")

  (ok (signals (function-operands 'a) 'simple-error)
      "FORM which is an invalid function application.")
  (is (function-operands '(foo)) '()
      "basic case 10")
  (is (function-operands '(+ 1 1)) '(1 1)
      "basic case 11")
  (is (function-operands '(foo 1 1)) '(1 1)
      "basic case 12"))


;;;
;;; test If statement
;;;

(deftest if-statement

  (is (if-else-statement '(if (= 1 1) (return 1))) nil
      "basic case 1"))


;;;
;;; test Let statement
;;;

(deftest let-statement
  (is (let-statements '(let ((alfa  0)))) nil)
  (is (let-statements '(let ((alfa  0)) alfa bravo)) '(alfa bravo)))

;;;
;;; test Symbol-macrolet statement
;;;

(deftest symbol-macrolet-statement

  (ok (symbol-macrolet-p '(symbol-macrolet ((x 'expanded-x))
                            (return)))
      "basic case 1")
  (ok (symbol-macrolet-p '(symbol-macrolet ((x 'expanded-x))
                            (do-something)
                            (return)))
      "basic case 2")
  (ok (symbol-macrolet-p '(symbol-macrolet ((x 'expanded-x))))
      "basic case 3"))


;;;
;;; test Do statement
;;;

(deftest do-statement

  (let ((code '(do ((a 0 (+ a 1))
                    (b 0 (+ b 1)))
                ((> a 15))
                (return))))
    (ok (do-p code)
        "basic case 1")
    (is (do-bindings code) '((a 0 (+ a 1))
                             (b 0 (+ b 1)))
        "basic case 2")
    (is (do-end-test code) '(> a 15)
        "basic case 3")
    (is (do-statements code) '((return))
        "basic case 4")))

(deftest do-statement-binding

  (let ((binding '(a 0 (+ a 1))))
    (ok (do-binding-p binding)
        "basic case 1")
    (is (do-binding-var binding) 'a
        "basic case 2")
    (is (do-binding-init binding) 0
        "basic case 3")
    (is (do-binding-step binding) '(+ a 1)
        "basic case 4")))


;;;
;;; test With-local-memory statement
;;;

(deftest with-local-memory-p

  (ok (with-local-memory-p '(with-local-memory ((a float 16))
                              (return)))
      "basic case 1")
  (ok (with-local-memory-p '(with-local-memory ()
                              (return)))
      "basic case 2")
  (ok (with-local-memory-p '(with-local-memory ()))
      "basic case 3")
  (ok (with-local-memory-p '(with-local-memory))
      "basic case 4"))


(deftest with-local-memory-spec-p

  (ok (with-local-memory-spec-p '(a float 16))
      "basic case 1")
  (ok (with-local-memory-spec-p '(a float (+ 16 2)))
      "basic case 2"))


;;;
;;; test Set statement
;;;

(deftest set-statement
  (ok (set-p '(set x 1))
      "basic case 1")
  (ok (set-p '(set (aref x i) 1))
      "basic case 2"))



;;;
;;; test Progn statement
;;;

(deftest progn-statements
  (is (progn-statements '(progn alfa bravo)) '(alfa bravo)))

;;;
;;; test Return expr
;;;

(deftest return-expr
  (is (return-expr '(return alfa)) 'alfa))

;;;
;;; test Argument
;;;

