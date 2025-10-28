#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.compiler.type-of-expression
  (:use :cl
        :oclcl.lang.type
        :oclcl.lang.syntax
        :oclcl.lang.environment
        :oclcl.lang.built-in)
  (:import-from #:alexandria
                #:rcurry)
  (:export :type-of-expression))
(in-package :oclcl.lang.compiler.type-of-expression)


;;;
;;; Type of expression
;;;

(defun type-of-expression (form env)
  (cond
    ((%macro-p form env) (type-of-macro form env))
    ((%symbol-macro-p form env)
     (type-of-symbol-macro form env))
    ((literal-p form) (type-of-literal form))
    ((opencl-literal-p form) (type-of-opencl-literal form))
    ((reference-p form) (type-of-reference form env))
    ((inline-if-p form) (type-of-inline-if form env))
    ((sizeof-p form) (type-of-sizeof form))
    ((arithmetic-p form) (type-of-arithmetic form env))
    ((function-p form) (type-of-function form env))
    (t (error "The value ~S is an invalid expression." form))))


;;;
;;; Macro
;;;

(defun %macro-p (form env)
  (and (macro-p form)
       (function-environment-macro-exists-p env
                                            (macro-operator form))))

(defun type-of-macro (form env)
  (let ((name (macro-operator form))
        (arguments (macro-operands form)))
    (let ((expander (function-environment-macro-expander env name)))
      (let ((form1 (funcall expander arguments)))
        (type-of-expression form1 env)))))


;;;
;;; Symbol macro
;;;

(defun %symbol-macro-p (form env)
  (and (symbolp form)
       (variable-environment-symbol-macro-exists-p env form)))

(defun type-of-symbol-macro (form env)
  (let ((form1 (variable-environment-symbol-macro-expansion env form)))
    (type-of-expression form1 env)))


;;;
;;; Literal
;;;

(defun type-of-literal (form)
  (cond
    ((bool-literal-p form) 'bool)
    ((int-literal-p form) 'int)
    ((float-literal-p form) 'float)
    ((double-literal-p form) 'double)
    ((string-literal-p form) 'string)
    (t (error "The value ~S is an invalid expression." form))))

(defun type-of-opencl-literal (form)
  (cond
    ((cl-mem-fence-flags-literal-p form) 'cl-mem-fence-flags)
    (t (error "The value ~s is an invalid expression." form))))

;;;
;;; Reference
;;;

(defun type-of-reference (form env)
  (cond
    ((variable-reference-p form)
     (type-of-variable-reference form env))
    ((structure-reference-p form)
     (type-of-structure-reference form env))
    ((array-reference-p form)
     (type-of-array-reference form env))
    (t (error "The value ~S is an invalid expression." form))))


;;;
;;; Reference - Variable
;;;

(defun type-of-variable-reference (form env)
  (cond
    ((variable-environment-variable-exists-p env form)
     (variable-environment-variable-type env form))
    ((variable-environment-memory-exists-p env form)
     (variable-environment-memory-type env form))
    (t
     (error "The variable ~S not found." form))))

;;;
;;; Reference - Structure
;;;

(defun type-of-structure-reference (form env)
  (let ((accessor (structure-reference-accessor form))
        (expr (structure-reference-expr form)))
    ;; check if the expression part of structure reference has the
    ;; same type as accessor's structure
    (let ((structure (structure-from-accessor accessor))
          (expr-type (type-of-expression expr env)))
      (unless (eq structure expr-type)
        (error "The structure reference ~S is invalid." form)))
    (structure-accessor-return-type accessor)))


;;;
;;; Reference - Array
;;;

(defun type-of-array-reference (form env)
  (let ((expr (array-reference-expr form))
        (indices (array-reference-indices form)))
    (let ((expr-type (type-of-expression expr env)))
      ;; check if the expression part of array reference has the same
      ;; dimension as the array reference
      (unless (= (array-type-dimension expr-type) (length indices))
        (error "The dimension of array reference ~S is invalid." form))
      (array-type-base expr-type))))


;;;
;;; Inline-if
;;;

(defun type-of-inline-if (form env)
  (let ((test-expr (inline-if-test-expression form))
        (then-expr (inline-if-then-expression form))
        (else-expr (inline-if-else-expression form)))
    ;; check if the test part of inline-if expression has bool type
    (let ((test-type (type-of-expression test-expr env)))
      (unless (eq test-type 'bool)
        (error "The type of expression ~S is invalid." form)))
    (let ((then-type (type-of-expression then-expr env))
          (else-type (type-of-expression else-expr env)))
      ;; check if the then part of inline-of expression has the same
      ;; type as the else part of it
      (unless (eq then-type else-type)
        (error "The type of expression ~S is invalid." form))
      then-type)))


;;;
;;; Arithmetic operations
;;;

(defun type-of-arithmetic (form env)
  (let ((operator (arithmetic-operator form))
        (operands (arithmetic-operands form)))
    (if (<= (length operands) 2)
        (type-of-function form env)
        (let ((operand-head (car operands))
              (operand-tail (cdr operands)))
          (let ((form1 `(,operator ,operand-head
                                   (,operator ,@operand-tail))))
            (type-of-expression form1 env))))))


;;;
;;; Function application
;;;

(defun type-of-operands (operands env)
  (mapcar (rcurry #'type-of-expression env)
          operands))

(defun type-of-function (form env)
  (let ((operator (function-operator form)))
    (if (function-environment-function-exists-p env operator)
        (type-of-user-defined-function form env)
        (type-of-built-in-function form env))))

(defun type-of-user-defined-function (form env)
  (let ((operator (function-operator form))
        (operands (function-operands form)))
    ;; check if the operands have the same types as the operator expect
    (let ((expected (function-environment-function-argument-types
                      env operator))
          (actual (type-of-operands operands env)))
      (unless (equal expected actual)
        (error "The function application ~S is invalid." form)))
    (function-environment-function-return-type env operator)))

(defun type-of-built-in-function (form env)
  (let ((operator (function-operator form))
        (operands (function-operands form)))
    (let ((argument-types (type-of-operands operands env)))
      (built-in-function-return-type operator argument-types))))

;;;
;;; Sizeof
;;;

(defun type-of-sizeof(form env)
  'size-t)