#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.program
  (:use :cl
        :oclcl.lang.util
        :oclcl.lang.data
        :oclcl.lang.type
        :oclcl.lang.syntax)
  (:export ;; Program
           :program
           :make-program
           :program-function-names
           :program-macro-names
           :program-symbol-macro-names
           :program-memory-names
           ;; Memory
           :program-define-memory
           :program-memory-exists-p
           :program-memory-name
           :program-memory-c-name
           :program-address-space-qualifiers
           :program-memory-expression
           ;; Function
           :program-define-function
           :program-function-exists-p
           :program-function-name
           :program-function-c-name
           :program-function-return-type
           :program-function-arguments
           :program-function-argument-vars
           :program-function-argument-types
           :program-function-body
           ;; Macro
           :program-define-macro
           :program-macro-exists-p
           :program-macro-name
           :program-macro-arguments
           :program-macro-body
           :program-macro-expander
           :expand-macro-1
           :expand-macro
           ;; Symbol macro
           :program-define-symbol-macro
           :program-symbol-macro-exists-p
           :program-symbol-macro-name
           :program-symbol-macro-expansion)
  ;; Shadow symbols in oclcl.lang.syntax.
  (:shadow :macro-p
           :symbol-macro-p
           :function-p)
  (:import-from :alexandria
                :with-gensyms
                :ensure-list))
(in-package :oclcl.lang.program)


;;;
;;; Program definition
;;;

(defstruct program
  name
  (variable-namespace nil)
  (function-namespace nil))

(defun program-function-names (program)
  (let ((namespace (program-function-namespace program)))
    (loop for (name object) on namespace by #'cddr
       when (function-p object)
       collect name)))

(defun program-memory-names (program)
  (let ((namespace (program-variable-namespace program)))
    (nreverse
     (loop for (name object) on namespace by #'cddr
        when (memory-p object)
        collect name))))

(defun program-macro-names (program)
  (let ((namespace (program-function-namespace program)))
    (loop for (name object) on namespace by #'cddr
       when (macro-p object)
       collect name)))

(defun program-symbol-macro-names (program)
  (let ((namespace (program-variable-namespace program)))
    (loop for (name object) on namespace by #'cddr
       when (symbol-macro-p object)
       collect name)))

;;; Memory
;;;

(deftype variable-qualifier ()
  `(satisfies variable-qualifier-p))

(defun variable-qualifier-p (object)
  (and (member object '(:global :local :constant :private))
       t))

(defstruct (memory (:constructor %make-memory))
  (name :name :read-only t)
  (qualifiers :qualifiers :read-only t)
  (expression :expression :read-only t))

(defun make-memory (name qualifiers expression)
  (let ((qualifiers1 (ensure-list qualifiers)))
    ;; Check type of name.
    (check-type name oclcl-symbol)
    ;; Check type of qualifiers.
    (loop for qualifier in qualifiers1
       do (check-type qualifier variable-qualifier))
    ;; Make memory.
    (%make-memory :name name
                  :qualifiers qualifiers1
                  :expression expression)))

(defun memory-c-name (memory)
  (c-identifier (memory-name memory) t))

;;; Global memory definition
;;;

(defun program-define-memory (program name qualifiers expression)
  (symbol-macrolet ((namespace (program-variable-namespace program)))
    (let ((memory (make-memory name qualifiers expression)))
      (setf (getf namespace name) memory)))
  name)

(defun program-memory-exists-p (program name)
  (check-type name oclcl-symbol)
  (let ((namespace (program-variable-namespace program)))
    (memory-p (getf namespace name))))

(defun %lookup-memory (program name)
  (unless (program-memory-exists-p program name)
    (error "The memory ~S not found." name))
  (let ((namespace (program-variable-namespace program)))
    (getf namespace name)))

(defun program-memory-name (program name)
  (memory-name (%lookup-memory program name)))

(defun program-memory-c-name (program name)
  (memory-c-name (%lookup-memory program name)))

(defun program-address-space-qualifiers (program name)
  (memory-qualifiers (%lookup-memory program name)))

(defun program-memory-expression (program name)
  (memory-expression (%lookup-memory program name)))

;;;
;;; Kernel function definition
;;;

(defun program-define-function (program name return-type arguments body)
  (symbol-macrolet ((namespace (program-function-namespace program)))
    (let ((function (make-function name return-type arguments body)))
      (setf (getf namespace name) function)))
  name)

(defun program-function-exists-p (program name)
  (let ((namespace (program-function-namespace program)))
    (function-p (getf namespace name))))

(defun %lookup-function (program name)
  (unless (program-function-exists-p program name)
    (error "The function ~S is undefined." name))
  (let ((namespace (program-function-namespace program)))
    (getf namespace name)))

(defun program-function-name (program name)
  (function-name (%lookup-function program name)))

(defun program-function-c-name (program name)
  (function-c-name (%lookup-function program name)))

(defun program-function-return-type (program name)
  (function-return-type (%lookup-function program name)))

(defun program-function-arguments (program name)
  (function-arguments (%lookup-function program name)))

(defun program-function-argument-vars (program name)
  (mapcar #'argument-var
    (program-function-arguments program name)))

(defun program-function-argument-types (program name)
  (mapcar #'argument-type
    (program-function-arguments program name)))

(defun program-function-body (program name)
  (function-body (%lookup-function program name)))

;;;
;;; Program definition - macro
;;;

(defun program-define-macro (program name arguments body)
  (symbol-macrolet ((namespace (program-function-namespace program)))
    (let ((macro (make-macro name arguments body)))
      (setf (getf namespace name) macro)))
  name)

(defun program-macro-exists-p (program name)
  (let ((namespace (program-function-namespace program)))
    (macro-p (getf namespace name))))

(defun %lookup-macro (program name)
  (unless (program-macro-exists-p program name)
    (error "The macro ~S is undefined." name))
  (let ((namespace (program-function-namespace program)))
    (getf namespace name)))

(defun program-macro-name (program name)
  (macro-name (%lookup-macro program name)))

(defun program-macro-arguments (program name)
  (macro-arguments (%lookup-macro program name)))

(defun program-macro-body (program name)
  (macro-body (%lookup-macro program name)))

(defun program-macro-expander (program name)
  (macro-expander (%lookup-macro program name)))

(defun expand-macro-1 (form program)
  (cond
    ((oclcl.lang.syntax:macro-p form)
     (let ((operator (macro-operator form))
           (operands (macro-operands form)))
       (if (program-macro-exists-p program operator)
           (let ((expander (program-macro-expander program operator)))
             (values (funcall expander operands) t))
           (values form nil))))
    ((oclcl.lang.syntax:symbol-macro-p form)
     (if (program-symbol-macro-exists-p program form)
         (let ((expansion (program-symbol-macro-expansion program form)))
           (values expansion t))
         (values form nil)))
    (t (values form nil))))

(defun expand-macro (form program)
  (labels ((aux (form expanded-p)
             (multiple-value-bind (form1 newly-expanded-p)
                 (expand-macro-1 form program)
               (if newly-expanded-p
                   (aux form1 t)
                   (values form1 expanded-p)))))
    (aux form nil)))


;;;
;;; Symbol macro definition
;;;

(defun program-define-symbol-macro (program name expansion)
  (symbol-macrolet ((namespace (program-variable-namespace program)))
    (let ((symbol-macro (make-symbol-macro name expansion)))
      (setf (getf namespace name) symbol-macro)))
  name)

(defun program-symbol-macro-exists-p (program name)
  (let ((namespace (program-variable-namespace program)))
    (symbol-macro-p (getf namespace name))))

(defun %lookup-symbol-macro (program name)
  (unless (program-symbol-macro-exists-p program name)
    (error "The symbol macro ~S not found." name))
  (let ((namespace (program-variable-namespace program)))
    (getf namespace name)))

(defun program-symbol-macro-name (program name)
  (symbol-macro-name (%lookup-symbol-macro program name)))

(defun program-symbol-macro-expansion (program name)
  (symbol-macro-expansion (%lookup-symbol-macro program name)))


;;;
;;; Function
;;;

;; use name begining with '%' to avoid package locking
(defstruct (%function (:constructor %make-function)
                      (:conc-name function-)
                      (:predicate function-p))
  (name :name :read-only t)
  (return-type :return-type :read-only t)
  (arguments :arguments :read-only t)
  (body :body :read-only t))

(defun make-function (name return-type arguments body)
  (unless (oclcl-symbol-p name)
    (error 'type-error :datum name :expected-type 'oclcl-symbol))
  (unless (oclcl-type-p return-type)
    (error 'type-error :datum return-type :expected-type 'oclcl-type))
  (dolist (argument arguments)
    (unless (argument-p argument)
      (error 'type-error :datum argument :expected-type 'argument)))
  (unless (listp body)
    (error 'type-error :datum body :expected-type 'list))
  (%make-function :name name
                  :return-type return-type
                  :arguments arguments
                  :body body))

(defun function-c-name (function)
  (c-identifier (function-name function) t))

(defun function-argument-vars (function)
  (mapcar #'argument-var
    (function-arguments function)))

(defun function-argument-types (function)
  (mapcar #'argument-type
    (function-arguments function)))


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
  (%make-macro :name name
               :arguments arguments
               :body body))

(defun macro-expander (macro)
  (let ((arguments (macro-arguments macro))
        (body (macro-body macro)))
    (with-gensyms (arguments1)
      (eval `#'(lambda (,arguments1)
                 (destructuring-bind ,arguments ,arguments1
                   ,@body))))))


;;;
;;; Symbol macro
;;;

(defstruct (symbol-macro (:constructor %make-symbol-macro))
  (name :name :read-only t)
  (expansion :expansion :read-only t))

(defun make-symbol-macro (name expansion)
  (unless (oclcl-symbol-p name)
    (error 'type-error :datum name :expected-type 'oclcl-symbol))
  (%make-symbol-macro :name name
                      :expansion expansion))
