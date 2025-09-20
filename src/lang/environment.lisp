#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.environment
  (:use :cl
        :oclcl.lang.util
        :oclcl.lang.data
        :oclcl.lang.type)
  (:export ;; Variable environment
           :empty-variable-environment
           ;; Variable environment - Variable
           :variable-environment-add-variable
           :variable-environment-variable-exists-p
           :variable-environment-variable-name
           :variable-environment-variable-type
           ;; Variable environment - Symbol macro
           :variable-environment-add-symbol-macro
           :variable-environment-symbol-macro-exists-p
           :variable-environment-symbol-macro-name
           :variable-environment-symbol-macro-expansion
           ;; Function environment
           :empty-function-environment
           ;; Function environment - Function
           :function-environment-add-function
           :function-environment-function-exists-p
           :function-environment-function-name
           :function-environment-function-c-name
           :function-environment-function-return-type
           :function-environment-function-argument-types
           ;; Variable environment - Memory
           :variable-environment-add-memory
           :variable-environment-memory-exists-p
           :variable-environment-memory-name
           :variable-environment-memory-c-name
           :variable-environment-memory-type
           :variable-environment-memory-expression
           ;; Function environment - Macro
           :function-environment-add-macro
           :function-environment-macro-exists-p
           :function-environment-macro-name
           :function-environment-macro-expander
           ;; Define environment
           :empty-define-environment
           ;; Define environment - Define
           :define-environment-add-define
           :define-environment-define-exists-p
           :define-environment-define-name
           :define-environment-define-c-name
           :define-environment-define-expression
           ;; Other
           :empty-environment)
  (:shadow :variable)
  (:import-from :alexandria
                :with-gensyms))
(in-package :oclcl.lang.environment)


;;;
;;; Variable environment
;;;

(defun empty-variable-environment ()
  nil)


;;;
;;; Variable environment - Variable
;;;

(defun variable-environment-add-variable (name type var-env)
  (let ((elem (make-variable name type)))
    (acons name elem var-env)))

(defun variable-environment-variable-exists-p (var-env name)
  (variable-p (cdr (assoc name var-env))))

(defun %lookup-variable (var-env name)
  (unless (variable-environment-variable-exists-p var-env name)
    (error "The variable ~S not found." name))
  (cdr (assoc name var-env)))

(defun variable-environment-variable-name (var-env name)
  (variable-name (%lookup-variable var-env name)))

(defun variable-environment-variable-type (var-env name)
  (variable-type (%lookup-variable var-env name)))


;;;
;;; Variable environment - Symbol macro
;;;

(defun variable-environment-add-symbol-macro (name expansion var-env)
  (let ((elem (make-symbol-macro name expansion)))
    (acons name elem var-env)))

(defun variable-environment-symbol-macro-exists-p (var-env name)
  (symbol-macro-p (cdr (assoc name var-env))))

(defun %lookup-symbol-macro (var-env name)
  (unless (variable-environment-symbol-macro-exists-p var-env name)
    (error "The symbol macro ~S not found." name))
  (cdr (assoc name var-env)))

(defun variable-environment-symbol-macro-name (var-env name)
  (symbol-macro-name (%lookup-symbol-macro var-env name)))

(defun variable-environment-symbol-macro-expansion (var-env name)
  (symbol-macro-expansion (%lookup-symbol-macro var-env name)))

;;; Variable environment - Memory
;;;

(defun variable-environment-add-memory (name type expression var-env)
  (check-type var-env list)
  (let ((elem (make-memory name type expression)))
    (acons name elem var-env)))

(defun variable-environment-memory-exists-p (var-env name)
  (check-type name oclcl-symbol)
  (memory-p (cdr (assoc name var-env))))

(defun %lookup-memory (var-env name)
  (unless (variable-environment-memory-exists-p var-env name)
    (error "The variable ~S not found." name))
  (cdr (assoc name var-env)))

(defun variable-environment-memory-name (var-env name)
  (memory-name (%lookup-memory var-env name)))

(defun variable-environment-memory-c-name (var-env name)
  (memory-c-name (%lookup-memory var-env name)))

(defun variable-environment-memory-type (var-env name)
  (memory-type (%lookup-memory var-env name)))

(defun variable-environment-memory-expression (var-env name)
  (memory-expression (%lookup-memory var-env name)))

;;;
;;; Function environment
;;;

(defun empty-function-environment ()
  '())


;;;
;;; Function environment - Function
;;;

(defun function-environment-add-function (name return-type
                                          argument-types func-env)
  (let ((elem (make-function name return-type argument-types)))
    (acons name elem func-env)))

(defun function-environment-function-exists-p (func-env name)
  (function-p (cdr (assoc name func-env))))

(defun %lookup-function (func-env name)
  (unless (function-environment-function-exists-p func-env name)
    (error "The function ~S is undefined." name))
  (cdr (assoc name func-env)))

(defun function-environment-function-name (func-env name)
  (function-name (%lookup-function func-env name)))

(defun function-environment-function-c-name (func-env name)
  (function-c-name (%lookup-function func-env name)))

(defun function-environment-function-return-type (func-env name)
  (function-return-type (%lookup-function func-env name)))

(defun function-environment-function-argument-types (func-env name)
  (function-argument-types (%lookup-function func-env name)))


;;;
;;; Function environment - Macro
;;;

(defun function-environment-add-macro (name arguments body func-env)
  (let ((elem (make-macro name arguments body)))
    (acons name elem func-env)))

(defun function-environment-macro-exists-p (func-env name)
  (macro-p (cdr (assoc name func-env))))

(defun %lookup-macro (func-env name)
  (unless (function-environment-macro-exists-p func-env name)
    (error "The macro ~S is undefined." name))
  (cdr (assoc name func-env)))

(defun function-environment-macro-name (func-env name)
  (macro-name (%lookup-macro func-env name)))

(defun function-environment-macro-expander (func-env name)
  (macro-expander (%lookup-macro func-env name)))


;;;
;;; Variable
;;;

(defstruct (variable (:constructor %make-variable))
  (name :name :read-only t)
  (type :type :read-only t))

(defun make-variable (name type)
  (unless (oclcl-symbol-p name)
    (error 'type-error :datum name :expected-type 'oclcl-symbol))
  (unless (oclcl-type-p type)
    (error 'type-error :datum type :expected-type 'oclcl-type))
  (%make-variable :name name :type type))


;;;
;;; Symbol macro
;;;

(defstruct (symbol-macro (:constructor %make-symbol-macro))
  (name :name :read-only t)
  (expansion :expansion :read-only t))

(defun make-symbol-macro (name expansion)
  (unless (oclcl-symbol-p name)
    (error 'type-error :datum name :expected-type 'oclcl-symbol))
  (%make-symbol-macro :name name :expansion expansion))


;;; Memory
;;;

(defstruct (memory (:constructor %make-memory))
  (name :name :read-only t)
  (type :type :read-only t)
  (expression :expression :read-only t))

(defun make-memory (name type expression)
  (check-type name oclcl-symbol)
  (check-type type oclcl-type)
  (%make-memory :name name :type type :expression expression))

(defun memory-c-name (memory)
  (c-identifier (memory-name memory) t))

;;;
;;; Function
;;;

;; use name begining with '%' to avoid package locking
(defstruct (%function (:constructor %make-function)
                      (:conc-name function-)
                      (:predicate function-p))
  (name :name :read-only t)
  (return-type :return-type :read-only t)
  (argument-types :argument-types :read-only t))

(defun make-function (name return-type argument-types)
  (unless (oclcl-symbol-p name)
    (error 'type-error :datum name :expected-type 'oclcl-symbol))
  (unless (oclcl-type-p return-type)
    (error 'type-error :datum return-type :expected-type 'oclcl-type))
  (dolist (argument-type argument-types)
    (unless (oclcl-type-p argument-type)
      (error 'type-error :datum argument-type
                         :expected-type 'oclcl-type)))
  (%make-function :name name
                  :return-type return-type
                  :argument-types argument-types))

(defun function-c-name (function)
  (c-identifier (function-name function) t))


;;;
;;; Macro
;;;

(defstruct (macro (:constructor %make-macro))
  (name :name :read-only t)
  (arguments :arguments :read-only t)
  (body :body :read-only t))

(defun make-macro (name arguments body)
  (unless (oclcl-symbol-p name)
    (error 'type-error :datum name :expected-type 'oclcl-symbol))
  (unless (listp arguments)
    (error 'type-error :datum arguments :expected-type 'list))
  (unless (listp body)
    (error 'type-error :datum body :expected-type 'list))
  (%make-macro :name name :arguments arguments :body body))

(defun macro-expander (macro)
  (let ((arguments (macro-arguments macro))
        (body (macro-body macro)))
    (with-gensyms (arguments1)
      (eval `#'(lambda (,arguments1)
                 (destructuring-bind ,arguments ,arguments1
                   ,@body))))))

;;;
;;; Define environment - Define
;;;

(defun empty-define-environment ())

(defun define-environment-add-define (name expression def-env)
  (check-type def-env list)
  (let ((elem (make-define name expression)))
    (acons name elem def-env)))

(defun define-environment-define-exists-p (def-env name)
  (check-type name oclcl-symbol)
  (define-p (cdr (assoc name def-env))))

(defun %lookup-define (def-env name)
  (unless (define-environment-define-exists-p def-env name)
    (error "The define ~S not found." name))
  (cdr (assoc name def-env)))

(defun define-environment-define-name (def-env name)
  (define-name (%lookup-define def-env name)))

(defun define-environment-define-c-name (def-env name)
  (define-c-name (%lookup-define def-env name)))

(defun define-environment-define-expression (def-env name)
  (define-expression (%lookup-define def-env name)))

;;; Define
;;;

(defstruct (define (:constructor %make-define))
  (name :name :read-only t)
  (expression :expression :read-only t))

(defun make-define (name expression)
  (check-type name oclcl-symbol)
  (%make-define :name name :expression expression))

(defun define-c-name (define)
  (c-identifier (define-name define) t))

;;; Empty environment
;;;

(defun empty-environment ()
  (values (empty-variable-environment)
          (empty-function-environment)
          (empty-define-environment)))
