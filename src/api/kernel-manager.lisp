#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.api.kernel-manager
  (:use :cl
        :oclcl.lang.kernel
        :oclcl.lang.compiler.compile-kernel)
  (:export :kernel-manager
           :make-kernel-manager
           :kernel-manager-define-function
           :kernel-manager-define-macro
           :kernel-manager-define-symbol-macro
           :kernel-manager-define-memory
           :kernel-manager-translate
           :kernel-manager-function-c-name
           :expand-macro-1
           :expand-macro
           :*kernel-manager*)
  (:shadow :expand-macro-1
           :expand-macro)
  (:import-from :alexandria
                :ensure-list)
  (:documentation
   "Defines the kernel-manager object.

The special variable *kernel-manager* is bound to a kernel-manager instance.
Kernel managers store the various information required to access the defined OCLCL entities.
"))
(in-package :oclcl.api.kernel-manager)


;;;
;;; Kernel manager
;;;

(defstruct (kernel-manager (:constructor %make-kernel-manager))
  name
  kernel)

(defun make-kernel-manager (&optional (name (gensym)))
  (%make-kernel-manager :name name
                        :kernel (make-kernel)))

(defun kernel-manager-define-function (manager name return-type arguments body)
  (symbol-macrolet ((kernel (kernel-manager-kernel manager)))
    (when (function-modified-p kernel name return-type arguments body)
      (kernel-define-function kernel name return-type arguments body)))
  name)

(defun function-modified-p (kernel name return-type arguments body)
  (not (and (kernel-function-exists-p kernel name)
            (equal return-type (kernel-function-return-type kernel name))
            (equal arguments (kernel-function-arguments kernel name))
            (equal body (kernel-function-body kernel name)))))

(defun kernel-manager-define-macro (manager name arguments body)
  (symbol-macrolet ((kernel (kernel-manager-kernel manager)))
    (when (macro-modified-p kernel name arguments body)
      (kernel-define-macro kernel name arguments body)))
  name)

(defun macro-modified-p (kernel name arguments body)
  (not (and (kernel-macro-exists-p kernel name)
            (equal arguments (kernel-macro-arguments kernel name))
            (equal body (kernel-macro-body kernel name)))))

(defun kernel-manager-define-symbol-macro (manager name expansion)
  (symbol-macrolet ((kernel (kernel-manager-kernel manager)))
    (when (symbol-macro-modified-p kernel name expansion)
      (kernel-define-symbol-macro kernel name expansion)))
  name)

(defun symbol-macro-modified-p (kernel name expansion)
  (not (and (kernel-symbol-macro-exists-p kernel name)
            (equal expansion (kernel-symbol-macro-expansion kernel name)))))

(defun kernel-manager-define-memory (manager name qualifiers
                                     &optional expression)
  (symbol-macrolet ((kernel (kernel-manager-kernel manager)))
    (when (memory-modified-p kernel name qualifiers expression)
      (kernel-define-memory kernel name qualifiers expression)))
  name)

(defun memory-modified-p (kernel name qualifiers expression)
  (not (and (kernel-memory-exists-p kernel name)
            (equal (ensure-list qualifiers)
                   (kernel-address-space-qualifiers kernel name))
            (equal expression (kernel-memory-expression kernel name)))))

(defun kernel-manager-translate (manager)
  (let ((kernel (kernel-manager-kernel manager)))
    (compile-kernel kernel)))

(defun kernel-manager-function-c-name (manager name)
  (kernel-function-c-name (kernel-manager-kernel manager)
                          name))

(defun expand-macro-1 (form manager)
  (let ((kernel (kernel-manager-kernel manager)))
    (oclcl.lang.kernel:expand-macro-1 form kernel)))

(defun expand-macro (form manager)
  (let ((kernel (kernel-manager-kernel manager)))
    (oclcl.lang.kernel:expand-macro form kernel)))

(defvar *kernel-manager*)

