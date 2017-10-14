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
        :oclcl.api.kernel-manager)
  (:export :defkernel
           :defmemory
           :defkernelmacro
           :expand-macro-1
           :expand-macro
           :defkernel-symbol-macro
           :in-kernel-module
           :define-kernel-module)
  (:shadow :expand-macro-1
           :expand-macro)
  (:import-from :alexandria
                :format-symbol
                :with-gensyms
                :simple-style-warning)
  (:documentation
   "Defines some convenience wrapper macros that register the OpenCL objects,
 such as kernels, memory and kernel macros.
 For the actual definitions see kernel-manager.lisp"))
(in-package :oclcl.api.defkernel)

(lispn:define-namespace kernel-module t nil "Namespace for the kernel module.

 A kernel module is a single compilation unit, i.e. a single OpenCL C source code.
 An instance of kernel module is (rather inadequetely) named a kernel-manager.
")

(defmacro define-kernel-module (name &body options)
  "define-kernel-module creates a kernel-module as specified and returns the kernel-module. "
  (declare (ignorable options))         ;; for future extensions
  `(setf (symbol-kernel-module ',name)
         (make-kernel-manager ',name)))

(defmacro in-kernel-module (name)
  "NAME is a symbol, not evaluated.

Causes the the kernel-module named by NAME to become the current kernel-module
--- that is, the value of *kernel-manager*. If no such package already exists, an error
of type package-error is signaled.
"
  `(setf *kernel-manager* (symbol-kernel-module ',name)))

;;;
;;; DEFKERNEL
;;;


(defmacro defkernel (name (return-type arguments) &body body)
  "Register the kernel definition to *KERNEL-MANAGER* ."
  `(kernel-manager-define-function *kernel-manager*
                                   ',name
                                   ',return-type
                                   ',arguments
                                   ',body))

;;; DEFMEMORY
;;;

(defmacro defmemory (name expression &optional qualifiers)
  "Register the name and the initialization statement of a global variable (stored in the global memory)
 to *KERNEL-MANAGER* ."
  `(kernel-manager-define-memory *kernel-manager*
                                 ',name
                                 ',(or qualifiers :global)
                                 ',expression))

;;;
;;; DEFKERNELMACRO
;;;

(defmacro defkernelmacro (name arguments &body body)
  "Register the kernel macro definition to *KERNEL-MANAGER* .
For a macro which is not fbound, it tries to define the regular CL macro so that SLIME macroexpansion and
eldoc works."
  (with-gensyms (e)
    `(progn
       ,(if (fboundp name)
            (simple-style-warning "Could not define the kernel macro ~a also as a regular macro, because it is fbound." name)
            `(defmacro ,name (,@arguments &environment ,e)
               ,@body))
       (kernel-manager-define-macro *kernel-manager* ',name ',arguments ',body))))

(defun expand-macro-1 (form)
  "Equivalent to macroexpand-1"
  (oclcl.api.kernel-manager:expand-macro-1 form *kernel-manager*))

(defun expand-macro (form)
  "Equivalent to macroexpand"
  (oclcl.api.kernel-manager:expand-macro form *kernel-manager*))


;;;
;;; DEFKERNEL-SYMBOL-MACRO
;;;

(defmacro defkernel-symbol-macro (name expansion)
  "Register the kernel symbol macro to *KERNEL-MANAGER*."
  `(kernel-manager-define-symbol-macro *kernel-manager* ',name ',expansion))
