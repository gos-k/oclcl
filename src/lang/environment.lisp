#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.environment
  (:use :cl
        :oclcl.lang.util
        :oclcl.lang.data
        :oclcl.lang.type)
  (:export ;; Environment
           :environment
           :make-environment
           :copy-environment
           :environment-p
           :environment-var-env
           :environment-func-env
           :environment-def-env
           :empty-environment
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
           ;; Define environment - Define
           :define-environment-add-define
           :define-environment-define-exists-p
           :define-environment-define-name
           :define-environment-define-c-name
           :define-environment-define-expression)
  (:shadow :variable)
  (:import-from :alexandria
                :with-gensyms))
(in-package :oclcl.lang.environment)


;;;
;;; Environment
;;;

(defstruct environment
  (var-env nil :type list)
  (func-env nil :type list)
  (def-env nil :type list))

(defun empty-environment ()
  (make-environment))

(defun copy-env-with-var-env (env new-var-env)
  (make-environment :var-env new-var-env
                    :func-env (environment-func-env env)
                    :def-env (environment-def-env env)))

(defun copy-env-with-func-env (env new-func-env)
  (make-environment :var-env (environment-var-env env)
                    :func-env new-func-env
                    :def-env (environment-def-env env)))

(defun copy-env-with-def-env (env new-def-env)
  (make-environment :var-env (environment-var-env env)
                    :func-env (environment-func-env env)
                    :def-env new-def-env))


;;;
;;; Variable environment - Variable
;;;

(defun variable-environment-add-variable (name type env)
  (copy-env-with-var-env
   env
   (acons name (make-variable name type) (environment-var-env env))))

(defun variable-environment-variable-exists-p (env name)
  (variable-p (cdr (assoc name (environment-var-env env)))))

(defun %lookup-variable (env name)
  (unless (variable-environment-variable-exists-p env name)
    (error "The variable ~S not found." name))
  (cdr (assoc name (environment-var-env env))))

(defun variable-environment-variable-name (env name)
  (variable-name (%lookup-variable env name)))

(defun variable-environment-variable-type (env name)
  (variable-type (%lookup-variable env name)))


;;;
;;; Variable environment - Symbol macro
;;;

(defun variable-environment-add-symbol-macro (name expansion env)
  (copy-env-with-var-env
   env
   (acons name (make-symbol-macro name expansion) (environment-var-env env))))

(defun variable-environment-symbol-macro-exists-p (env name)
  (symbol-macro-p (cdr (assoc name (environment-var-env env)))))

(defun %lookup-symbol-macro (env name)
  (unless (variable-environment-symbol-macro-exists-p env name)
    (error "The symbol macro ~S not found." name))
  (cdr (assoc name (environment-var-env env))))

(defun variable-environment-symbol-macro-name (env name)
  (symbol-macro-name (%lookup-symbol-macro env name)))

(defun variable-environment-symbol-macro-expansion (env name)
  (symbol-macro-expansion (%lookup-symbol-macro env name)))

;;; Variable environment - Memory
;;;

(defun variable-environment-add-memory (name type expression env)
  (copy-env-with-var-env
   env
   (acons name (make-memory name type expression) (environment-var-env env))))

(defun variable-environment-memory-exists-p (env name)
  (check-type name oclcl-symbol)
  (memory-p (cdr (assoc name (environment-var-env env)))))

(defun %lookup-memory (env name)
  (unless (variable-environment-memory-exists-p env name)
    (error "The variable ~S not found." name))
  (cdr (assoc name (environment-var-env env))))

(defun variable-environment-memory-name (env name)
  (memory-name (%lookup-memory env name)))

(defun variable-environment-memory-c-name (env name)
  (memory-c-name (%lookup-memory env name)))

(defun variable-environment-memory-type (env name)
  (memory-type (%lookup-memory env name)))

(defun variable-environment-memory-expression (env name)
  (memory-expression (%lookup-memory env name)))


;;;
;;; Function environment - Function
;;;

(defun function-environment-add-function (name return-type argument-types env)
  (copy-env-with-func-env
   env
   (acons name (make-function name return-type argument-types) (environment-func-env env))))

(defun function-environment-function-exists-p (env name)
  (function-p (cdr (assoc name (environment-func-env env)))))

(defun %lookup-function (env name)
  (unless (function-environment-function-exists-p env name)
    (error "The function ~S is undefined." name))
  (cdr (assoc name (environment-func-env env))))

(defun function-environment-function-name (env name)
  (function-name (%lookup-function env name)))

(defun function-environment-function-c-name (env name)
  (function-c-name (%lookup-function env name)))

(defun function-environment-function-return-type (env name)
  (function-return-type (%lookup-function env name)))

(defun function-environment-function-argument-types (env name)
  (function-argument-types (%lookup-function env name)))


;;;
;;; Function environment - Macro
;;;

(defun function-environment-add-macro (name arguments body env)
  (copy-env-with-func-env
   env
   (acons name (make-macro name arguments body) (environment-func-env env))))

(defun function-environment-macro-exists-p (env name)
  (macro-p (cdr (assoc name (environment-func-env env)))))

(defun %lookup-macro (env name)
  (unless (function-environment-macro-exists-p env name)
    (error "The macro ~S is undefined." name))
  (cdr (assoc name (environment-func-env env))))

(defun function-environment-macro-name (env name)
  (macro-name (%lookup-macro env name)))

(defun function-environment-macro-expander (env name)
  (macro-expander (%lookup-macro env name)))


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

(defun define-environment-add-define (name expression env)
  (copy-env-with-def-env
   env
   (acons name (make-define name expression) (environment-def-env env))))

(defun define-environment-define-exists-p (env name)
  (check-type name oclcl-symbol)
  (define-p (cdr (assoc name (environment-def-env env)))))

(defun %lookup-define (env name)
  (unless (define-environment-define-exists-p env name)
    (error "The define ~S not found." name))
  (cdr (assoc name (environment-def-env env))))

(defun define-environment-define-name (env name)
  (define-name (%lookup-define env name)))

(defun define-environment-define-c-name (env name)
  (define-c-name (%lookup-define env name)))

(defun define-environment-define-expression (env name)
  (define-expression (%lookup-define env name)))

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