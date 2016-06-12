#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.kernel
  (:use :cl
        :oclcl.lang.util
        :oclcl.lang.data
        :oclcl.lang.type
        :oclcl.lang.syntax)
  (:export ;; Kernel
           :make-kernel
           :kernel-function-names
           :kernel-macro-names
           :kernel-symbol-macro-names
           :kernel-memory-names
           ;; Global
           :kernel-define-memory
           :kernel-memory-exists-p
           :kernel-memory-name
           :kernel-memory-c-name
           :kernel-address-space-qualifiers
           :kernel-memory-expression
           ;; Function
           :kernel-define-function
           :kernel-function-exists-p
           :kernel-function-name
           :kernel-function-c-name
           :kernel-function-return-type
           :kernel-function-arguments
           :kernel-function-argument-vars
           :kernel-function-argument-types
           :kernel-function-body
           ;; Macro
           :kernel-define-macro
           :kernel-macro-exists-p
           :kernel-macro-name
           :kernel-macro-arguments
           :kernel-macro-body
           :kernel-macro-expander
           :expand-macro-1
           :expand-macro
           ;; Symbol macro
           :kernel-define-symbol-macro
           :kernel-symbol-macro-exists-p
           :kernel-symbol-macro-name
           :kernel-symbol-macro-expansion)
  ;; Shadow symbols in oclcl.lang.syntax.
  (:shadow :macro-p
           :symbol-macro-p
           :function-p)
  (:import-from :alexandria
                :with-gensyms
                :ensure-list))
(in-package :oclcl.lang.kernel)


;;;
;;; Kernel definition
;;;

(defstruct (kernel (:constructor %make-kernel))
  (variable-namespace :variable-namespace)
  (function-namespace :function-namespace))

(defun make-kernel ()
  (%make-kernel :variable-namespace '()
                :function-namespace '()))

(defun kernel-function-names (kernel)
  (let ((namespace (kernel-function-namespace kernel)))
    (loop for (name object) on namespace by #'cddr
       when (function-p object)
       collect name)))

(defun kernel-memory-names (kernel)
  (let ((namespace (kernel-variable-namespace kernel)))
    (nreverse
     (loop for (name object) on namespace by #'cddr
        when (memory-p object)
        collect name))))

(defun kernel-macro-names (kernel)
  (let ((namespace (kernel-function-namespace kernel)))
    (loop for (name object) on namespace by #'cddr
       when (macro-p object)
       collect name)))

(defun kernel-symbol-macro-names (kernel)
  (let ((namespace (kernel-variable-namespace kernel)))
    (loop for (name object) on namespace by #'cddr
       when (symbol-macro-p object)
       collect name)))

;;; Global
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
    ;; Make global.
    (%make-memory :name name
                  :qualifiers qualifiers1
                  :expression expression)))

(defun memory-c-name (memory)
  (c-identifier (memory-name memory) t))

;;; Kernel definition - global
;;;

(defun kernel-define-memory (kernel name qualifiers expression)
  (symbol-macrolet ((namespace (kernel-variable-namespace kernel)))
    (let ((global (make-memory name qualifiers expression)))
      (setf (getf namespace name) global)))
  name)

(defun kernel-memory-exists-p (kernel name)
  (check-type name oclcl-symbol)
  (let ((namespace (kernel-variable-namespace kernel)))
    (memory-p (getf namespace name))))

(defun %lookup-memory (kernel name)
  (unless (kernel-memory-exists-p kernel name)
    (error "The global ~S not found." name))
  (let ((namespace (kernel-variable-namespace kernel)))
    (getf namespace name)))

(defun kernel-memory-name (kernel name)
  (memory-name (%lookup-memory kernel name)))

(defun kernel-memory-c-name (kernel name)
  (memory-c-name (%lookup-memory kernel name)))

(defun kernel-address-space-qualifiers (kernel name)
  (memory-qualifiers (%lookup-memory kernel name)))

(defun kernel-memory-expression (kernel name)
  (memory-expression (%lookup-memory kernel name)))

;;;
;;; Kernel definition - function
;;;

(defun kernel-define-function (kernel name return-type arguments body)
  (symbol-macrolet ((namespace (kernel-function-namespace kernel)))
    (let ((function (make-function name return-type arguments body)))
      (setf (getf namespace name) function)))
  name)

(defun kernel-function-exists-p (kernel name)
  (let ((namespace (kernel-function-namespace kernel)))
    (function-p (getf namespace name))))

(defun %lookup-function (kernel name)
  (unless (kernel-function-exists-p kernel name)
    (error "The function ~S is undefined." name))
  (let ((namespace (kernel-function-namespace kernel)))
    (getf namespace name)))

(defun kernel-function-name (kernel name)
  (function-name (%lookup-function kernel name)))

(defun kernel-function-c-name (kernel name)
  (function-c-name (%lookup-function kernel name)))

(defun kernel-function-return-type (kernel name)
  (function-return-type (%lookup-function kernel name)))

(defun kernel-function-arguments (kernel name)
  (function-arguments (%lookup-function kernel name)))

(defun kernel-function-argument-vars (kernel name)
  (mapcar #'argument-var
    (kernel-function-arguments kernel name)))

(defun kernel-function-argument-types (kernel name)
  (mapcar #'argument-type
    (kernel-function-arguments kernel name)))

(defun kernel-function-body (kernel name)
  (function-body (%lookup-function kernel name)))

;;;
;;; Kernel definition - macro
;;;

(defun kernel-define-macro (kernel name arguments body)
  (symbol-macrolet ((namespace (kernel-function-namespace kernel)))
    (let ((macro (make-macro name arguments body)))
      (setf (getf namespace name) macro)))
  name)

(defun kernel-macro-exists-p (kernel name)
  (let ((namespace (kernel-function-namespace kernel)))
    (macro-p (getf namespace name))))

(defun %lookup-macro (kernel name)
  (unless (kernel-macro-exists-p kernel name)
    (error "The macro ~S is undefined." name))
  (let ((namespace (kernel-function-namespace kernel)))
    (getf namespace name)))

(defun kernel-macro-name (kernel name)
  (macro-name (%lookup-macro kernel name)))

(defun kernel-macro-arguments (kernel name)
  (macro-arguments (%lookup-macro kernel name)))

(defun kernel-macro-body (kernel name)
  (macro-body (%lookup-macro kernel name)))

(defun kernel-macro-expander (kernel name)
  (macro-expander (%lookup-macro kernel name)))

(defun expand-macro-1 (form kernel)
  (cond
    ((oclcl.lang.syntax:macro-p form)
     (let ((operator (macro-operator form))
           (operands (macro-operands form)))
       (if (kernel-macro-exists-p kernel operator)
           (let ((expander (kernel-macro-expander kernel operator)))
             (values (funcall expander operands) t))
           (values form nil))))
    ((oclcl.lang.syntax:symbol-macro-p form)
     (if (kernel-symbol-macro-exists-p kernel form)
         (let ((expansion (kernel-symbol-macro-expansion kernel form)))
           (values expansion t))
         (values form nil)))
    (t (values form nil))))

(defun expand-macro (form kernel)
  (labels ((aux (form expanded-p)
             (multiple-value-bind (form1 newly-expanded-p)
                 (expand-macro-1 form kernel)
               (if newly-expanded-p
                   (aux form1 t)
                   (values form1 expanded-p)))))
    (aux form nil)))


;;;
;;; Kernel definition - symbol macro
;;;

(defun kernel-define-symbol-macro (kernel name expansion)
  (symbol-macrolet ((namespace (kernel-variable-namespace kernel)))
    (let ((symbol-macro (make-symbol-macro name expansion)))
      (setf (getf namespace name) symbol-macro)))
  name)

(defun kernel-symbol-macro-exists-p (kernel name)
  (let ((namespace (kernel-variable-namespace kernel)))
    (symbol-macro-p (getf namespace name))))

(defun %lookup-symbol-macro (kernel name)
  (unless (kernel-symbol-macro-exists-p kernel name)
    (error "The symbol macro ~S not found." name))
  (let ((namespace (kernel-variable-namespace kernel)))
    (getf namespace name)))

(defun kernel-symbol-macro-name (kernel name)
  (symbol-macro-name (%lookup-symbol-macro kernel name)))

(defun kernel-symbol-macro-expansion (kernel name)
  (symbol-macro-expansion (%lookup-symbol-macro kernel name)))


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
