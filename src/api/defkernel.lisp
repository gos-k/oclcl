#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage :oclcl.api.defkernel
  (:use :cl
        :oclcl.lang.syntax
        :oclcl.lang.type
        :oclcl.lang.program)
  (:export :defkernel
           :defmemory
           :defkernelmacro
           :expand-macro-1
           :expand-macro
           :defkernel-symbol-macro
           :in-program
           :define-program
           :*program*)
  (:shadow :expand-macro-1
           :expand-macro)
  (:import-from :alexandria
                :format-symbol
                :with-gensyms
                :simple-style-warning)
  (:documentation
   "Defines some convenience wrapper macros that register the OpenCL objects,
 such as kernels, memory and kernel macros."))
(in-package :oclcl.api.defkernel)

(lispn:define-namespace program program nil "Namespace for the programs.

 A program in OpenCL is a single compilation unit, i.e. a single OpenCL C source code. ")

(defvar *program*)

(defmacro define-program (name &body options)
  "define-program creates a program as specified and returns the program. "
  (declare (ignorable options))         ;; for future extensions
  `(setf (symbol-program ',name)
         (make-program :name ',name)))

(defmacro in-program (name)
  "NAME is a symbol, not evaluated.

Causes the the program named by NAME to become the current program
--- that is, the value of *program*. If no such package already exists, an error
of type package-error is signaled.
"
  `(setf *program* (symbol-program ',name)))

(defun expand-macro-1 (form &optional (program *program*))
  (oclcl.lang.program:expand-macro-1 form program))

(defun expand-macro (form &optional (program *program*))
  (oclcl.lang.program:expand-macro form program))

;;;
;;; DEFKERNEL
;;;


(defmacro defkernel (name (return-type arguments) &body body)
  "Register the kernel definition to *PROGRAM* ."
  `(program-define-function *program*
                            ',name
                            ',return-type
                            ',arguments
                            ',body))

;;; DEFMEMORY
;;;

(defmacro defmemory (name expression &optional qualifiers)
  "Register the name and the initialization statement of a global variable (stored in the global memory)
 to *PROGRAM* ."
  `(program-define-memory *program*
                          ',name
                          ',(or qualifiers :global)
                          ',expression))

;;;
;;; DEFKERNELMACRO
;;;

(defmacro defkernelmacro (name arguments &body body)
  "Register the kernel macro definition to *PROGRAM* .
For a macro which is not fbound, it tries to define the regular CL macro so that SLIME macroexpansion and
eldoc works."
  (with-gensyms (e)
    `(progn
       ,(if (fboundp name)
            (simple-style-warning "Could not define the kernel macro ~a also as a regular macro, because it is fbound." name)
            `(defmacro ,name (,@arguments &environment ,e)
               (declare (ignorable ,e))
               ,@body))
       (program-define-macro *program* ',name ',arguments ',body))))

;;;
;;; DEFKERNEL-SYMBOL-MACRO
;;;

(defmacro defkernel-symbol-macro (name expansion)
  "Register the kernel symbol macro to *PROGRAM*."
  `(program-define-symbol-macro *program* ',name ',expansion))
