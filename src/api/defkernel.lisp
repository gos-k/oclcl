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
           :defkernelmacro
           :expand-macro-1
           :expand-macro
           :defkernel-symbol-macro)
  (:shadow :expand-macro-1
           :expand-macro)
  (:import-from :alexandria
                :format-symbol
                :with-gensyms))
(in-package :oclcl.api.defkernel)


;;;
;;; DEFKERNEL
;;;


(defmacro defkernel (name (return-type arguments) &body body)
  (with-gensyms (local-manager opencl-c-code)
    `(progn
       (let ((,local-manager (make-kernel-manager)))
         (kernel-manager-define-function ,local-manager
                                         ',name
                                         ',return-type
                                         ',arguments
                                         ',body)
         (kernel-manager-define-function *kernel-manager*
                                         ',name
                                         ',return-type
                                         ',arguments
                                         ',body)
         (let ((,opencl-c-code (kernel-manager-translate ,local-manager)))
         (defun ,name () ,opencl-c-code))))))

;;;
;;; DEFKERNELMACRO
;;;

(defmacro defkernelmacro (name arguments &body body)
  `(kernel-manager-define-macro *kernel-manager* ',name ',arguments ',body))

(defun expand-macro-1 (form)
  (oclcl.api.kernel-manager:expand-macro-1 form *kernel-manager*))

(defun expand-macro (form)
  (oclcl.api.kernel-manager:expand-macro form *kernel-manager*))


;;;
;;; DEFKERNEL-SYMBOL-MACRO
;;;

(defmacro defkernel-symbol-macro (name expansion)
  `(kernel-manager-define-symbol-macro *kernel-manager* ',name ',expansion))
