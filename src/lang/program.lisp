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
           :*program*
           :program
           :make-program
           :find-program
           :use-program
           :define-program
           :in-program
           :program-variable-namespace
           :program-function-namespace
           :program-use-list
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
           :program-symbol-macro-expansion
           :*program*
           :program-conflict
           :program-bound-names
           :program-fbound-names
           :oclcl-program-error
           :undefined-program-function
           :unbound-program-variable)
  ;; Shadow symbols in oclcl.lang.syntax.
  (:shadow :macro-p
           :symbol-macro-p
           :function-p)
  (:import-from :alexandria
                :with-gensyms
                :ensure-list
                :mappend))
(in-package :oclcl.lang.program)


;;;
;;; Program definition
;;;

(defvar *program*)

(defstruct program
  name
  (variable-namespace nil)
  (function-namespace nil)
  (use-list nil))

(lispn:define-namespace program program nil "Namespace for OpenCL programs.

 A program in OpenCL is a single compilation unit, i.e. a single OpenCL C source code. ")

(defmacro define-program (name &body options)
  "define-program creates a program as specified and returns the program. "
  (declare (ignorable options))         ;; for future extensions
  `(%define-program ',name ',options))

(defun %define-program (name options)
  (let ((p (make-program :name name)))
    (dolist (p2 (cdr (assoc :use options)))
      (use-program p2 p))
    (setf (symbol-program name) p)))

(defmacro in-program (name)
  "NAME is a symbol, not evaluated.

Causes the the program named by NAME to become the current program
--- that is, the value of *program*. If no such package already exists, an error
of type package-error is signaled.
"
  `(setf *program* (symbol-program ',name)))

(defun find-program (program)
  "Analogous to find-package, but the designator definition strings/keywords are not ."
  (declare ((or symbol program) program))
  (etypecase program
    (symbol (symbol-program program))
    (program program)))

(define-condition program-conflict (simple-condition) ())

(defun use-program (program-to-use &optional (program *program*))
  "Analogous to use-package, but does not accept strings.
May cause program-conflict."
  (let ((program-to-use (find-program program-to-use)))
    ;; TODO: restarts for resolving conflict
    ;; TODO: shadowing mechanism
    (assert (null (intersection (program-fbound-names program-to-use)
                                (program-fbound-names program)))
            nil 'program-conflict
            ;; There can be a conflict between a function vs function, macro vs function etc.
            "~a and ~a have a conflicting definition for the following symbols:~%~a"
            program-to-use program (intersection (program-function-names program-to-use)
                                                 (program-function-names program)))
    (assert (null (intersection (program-bound-names program-to-use)
                                (program-bound-names program)))
            nil 'program-conflict
            "~a and ~a have a conflicting definition for the following symbols:~%~a"
            program-to-use program (intersection (program-function-names program-to-use)
                                                 (program-function-names program)))
    (push program-to-use (program-use-list program)))
  t)

(defun program-fbound-names (program)
  (append (loop for (name object) on (program-function-namespace program) by #'cddr collect name)
          (mappend #'program-function-names (program-use-list program))))

(defun program-bound-names (program)
  (append (loop for (name object) on (program-variable-namespace program) by #'cddr collect name)
          (mappend #'program-bound-names (program-use-list program))))

(define-condition oclcl-program-error (cell-error)
  ((program :initarg :program))
  (:report (lambda (c s)
             (with-slots (program) c
               (format s "The symbol ~S is unbound in program ~a."
                       (cell-error-name c) program)))))

(define-condition undefined-program-variable (oclcl-program-error) ())
(define-condition undefined-program-function (oclcl-program-error) ())

(defun %lookup-bound (program name &optional (error t))
  (check-type name oclcl-symbol)
  (labels ((rec (p)
             (or (getf (program-variable-namespace p) name)
                 (some #'rec (program-use-list p)))))
    (or (rec program)
        (when error
          (error 'undefined-program-variable :name name :program program)))))

(defun (setf %lookup-bound) (newval program name)
  (check-type name oclcl-symbol)
  (labels ((rec (p)
             (or (loop for (name2 . rest) on (program-variable-namespace program) by #'cddr
                    when (eq name name2)
                    do
                      (progn (setf (car rest) newval)
                             (return newval)))
                 (some #'rec (program-use-list p)))))
    (or (rec program)
        (setf (program-variable-namespace program)
              (list* name newval (program-variable-namespace program))))))

(defun %lookup-fbound (program name &optional (error t))
  (check-type name oclcl-symbol)
  (labels ((rec (p)
             (or (getf (program-function-namespace p) name)
                 (some #'rec (program-use-list p)))))
    (or (rec program)
        (when error
          (error 'undefined-program-function :name name :program program)))))

(defun (setf %lookup-fbound) (newval program name)
  (check-type name oclcl-symbol)
  (labels ((rec (p)
             (or (loop for (name2 . rest) on (program-function-namespace program) by #'cddr
                    when (eq name name2)
                    do
                      (progn (setf (car rest) newval)
                             (return newval)))
                 (some #'rec (program-use-list p)))))
    (or (rec program)
        (setf (program-function-namespace program)
              (list* name newval (program-function-namespace program))))))

;;; 

(defun program-memory-names (program)
  (append (loop for (name object) on (program-variable-namespace program) by #'cddr
             when (memory-p object)
             collect name)
          (mappend #'program-memory-names (program-use-list program))))

(defun program-function-names (program)
  (append (loop for (name object) on (program-function-namespace program) by #'cddr
             when (function-p object)
             collect name)
          (mappend #'program-function-names (program-use-list program))))

(defun program-macro-names (program)
  (append (loop for (name object) on (program-function-namespace program) by #'cddr
             when (macro-p object)
             collect name)
          (mappend #'program-macro-names (program-use-list program))))

(defun program-symbol-macro-names (program)
  (append (loop for (name object) on (program-variable-namespace program) by #'cddr
             when (symbol-macro-p object)
             collect name)
          (mappend #'program-symbol-macro-names (program-use-list program))))

;;; Memory

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


(defun %lookup-memory (program name)
  (let ((o (%lookup-bound program name)))
    (when (memory-p o)
      o)))

(defun (setf %lookup-memory) (newval program name)
  (let ((o (%lookup-bound program name nil)))
    (unless (memory-p o)
      (warn "Redefining a variable ~a which was previously a symbol-macro" name))
    (setf (%lookup-bound program name) newval)))

(defun program-define-memory (program name qualifiers expression)
  (setf (%lookup-memory program name) (make-memory name qualifiers expression))
  name)

(defun program-memory-exists-p (program name)
  (%lookup-memory program name))

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

(defun %lookup-function (program name)
  (let ((o (%lookup-fbound program name)))
    (when (function-p o)
      o)))

(defun (setf %lookup-function) (newval program name)
  (let ((o (%lookup-fbound program name nil)))
    (unless (function-p o)
      (warn "Redefining a function ~a which was previously a macro" name))
    (setf (%lookup-fbound program name) newval)))

(defun program-define-function (program name return-type arguments body)
  (setf (%lookup-function program name) (make-function name return-type arguments body))
  name)

(defun program-function-exists-p (program name)
  (%lookup-function program name))

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

(defun %lookup-macro (program name)
  (let ((o (%lookup-fbound program name)))
    (when (macro-p o)
      o)))

(defun (setf %lookup-macro) (newval program name)
  (let ((o (%lookup-fbound program name nil)))
    (unless (macro-p o)
      (warn "Redefining a macro ~a which was previously a function" name))
    (setf (%lookup-fbound program name) newval)))

(defun program-define-macro (program name arguments body)
  (setf (%lookup-macro program name) (make-macro name arguments body))
  name)

(defun program-macro-exists-p (program name)
  (%lookup-macro program name))

(defun program-macro-name (program name)
  (macro-name (%lookup-macro program name)))

(defun program-macro-arguments (program name)
  (macro-arguments (%lookup-macro program name)))

(defun program-macro-body (program name)
  (macro-body (%lookup-macro program name)))

(defun program-macro-expander (program name)
  (macro-expander (%lookup-macro program name)))

(defun expand-macro-1 (form &optional (program *program*))
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

(defun expand-macro (form &optional (program *program*))
  (labels ((aux (form expanded-p)
             (multiple-value-bind (form1 newly-expanded-p)
                 (expand-macro-1 form program)
               (if newly-expanded-p
                   (aux form1 t)
                   (values form1 expanded-p)))))
    (aux form nil)))


;;;
;;; Symbol macro definition

(defun %lookup-symbol-macro (program name)
  (let ((o (%lookup-bound program name)))
    (when (symbol-macro-p o)
      o)))

(defun (setf %lookup-symbol-macro) (newval program name)
  (let ((o (%lookup-bound program name nil)))
    (unless (symbol-macro-p o)
      (warn "Redefining a variable ~a which was previously a symbol-macro" name))
    (setf (%lookup-bound program name) newval)))

(defun program-define-symbol-macro (program name expansion)
  (setf (%lookup-symbol-macro program name) (make-symbol-macro name expansion))
  name)

(defun program-symbol-macro-exists-p (program name)
  (%lookup-symbol-macro program name))

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
