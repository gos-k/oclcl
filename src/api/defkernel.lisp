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
           :defkernel-symbol-macro)
  (:import-from :alexandria
                :format-symbol
                :with-gensyms
                :simple-style-warning)
  (:documentation
   "Defines some convenience wrapper macros that register the OpenCL objects,
 such as kernels, memory and kernel macros."))
(in-package :oclcl.api.defkernel)

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
