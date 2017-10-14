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
           :kernel-manager-compiled-p
           :kernel-manager-module-handle
           :kernel-manager-function-handles-empty-p
           :kernel-manager-function-handle
           :kernel-manager-memory-objects-empty-p
           :kernel-manager-memory-object
           :kernel-manager-memory-qualifiers
           :kernel-manager-define-function
           :kernel-manager-define-macro
           :kernel-manager-define-symbol-macro
           :kernel-manager-define-memory
           :kernel-manager-load-global
           :kernel-manager-translate
           :kernel-manager-function-c-name
           :kernel-manager-unload
           :ensure-kernel-module-compiled
           :ensure-kernel-module-loaded
           :ensure-kernel-function-loaded
           :ensure-kernel-global-loaded
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
  module-path
  module-handle
  %function-handles
  %memory-objects
  kernel)

(defun make-kernel-manager (&optional (name (gensym)))
  (%make-kernel-manager :name name
                        :module-path nil
                        :module-handle nil
                        :%function-handles (make-hash-table)
                        :%memory-objects (make-hash-table)
                        :kernel (make-kernel)))

(defun kernel-manager-%function-handle (manager name)
  (let ((function-handles (kernel-manager-%function-handles manager)))
    (gethash name function-handles)))

(defun (setf kernel-manager-%function-handle) (value manager name)
  (let ((function-handles (kernel-manager-%function-handles manager)))
    (setf (gethash name function-handles) value)))

(defun kernel-manager-%memory-object (manager name)
  (let ((memory-objects (kernel-manager-%memory-objects manager)))
    (gethash name memory-objects)))

(defun (setf kernel-manager-%memory-object) (value manager name)
  (let ((memory-objects (kernel-manager-%memory-objects manager)))
    (setf (gethash name memory-objects) value)))

(defun kernel-manager-compiled-p (manager)
  (and (kernel-manager-module-path manager)
       t))

(defun kernel-manager-function-handles-empty-p (manager)
  (let ((function-handles (kernel-manager-%function-handles manager)))
    (zerop (hash-table-count function-handles))))

(defun kernel-manager-function-handle (manager name)
  (kernel-manager-%function-handle manager name))

(defun kernel-manager-memory-objects-empty-p (manager)
  (let ((memory-objects (kernel-manager-%memory-objects manager)))
    (zerop (hash-table-count memory-objects))))

(defun kernel-manager-memory-object (manager name)
  (kernel-manager-%memory-object manager name))

(defun kernel-manager-memory-qualifiers (manager name)
  (let ((kernel (kernel-manager-kernel manager)))
    (kernel-address-space-qualifiers kernel name)))

(defun kernel-manager-define-function (manager name return-type arguments body)
  (unless (not (kernel-manager-module-handle manager))
    (error "The kernel manager has already loaded the kernel module."))
  (symbol-macrolet ((module-path (kernel-manager-module-path manager))
                    (kernel (kernel-manager-kernel manager)))
    (when (function-modified-p kernel name return-type arguments body)
      (kernel-define-function kernel name return-type arguments body)
      (setf module-path nil)))
  name)

(defun function-modified-p (kernel name return-type arguments body)
  (not (and (kernel-function-exists-p kernel name)
            (equal return-type (kernel-function-return-type kernel name))
            (equal arguments (kernel-function-arguments kernel name))
            (equal body (kernel-function-body kernel name)))))

(defun kernel-manager-define-macro (manager name arguments body)
  (unless (not (kernel-manager-module-handle manager))
    (error "The kernel manager has already loaded the kernel module."))
  (symbol-macrolet ((module-path (kernel-manager-module-path manager))
                    (kernel (kernel-manager-kernel manager)))
    (when (macro-modified-p kernel name arguments body)
      (kernel-define-macro kernel name arguments body)
      (setf module-path nil)))
  name)

(defun macro-modified-p (kernel name arguments body)
  (not (and (kernel-macro-exists-p kernel name)
            (equal arguments (kernel-macro-arguments kernel name))
            (equal body (kernel-macro-body kernel name)))))

(defun kernel-manager-define-symbol-macro (manager name expansion)
  (unless (not (kernel-manager-module-handle manager))
    (error "The kernel manager has already loaded the kernel module."))
  (symbol-macrolet ((module-path (kernel-manager-module-path manager))
                    (kernel (kernel-manager-kernel manager)))
    (when (symbol-macro-modified-p kernel name expansion)
      (kernel-define-symbol-macro kernel name expansion)
      (setf module-path nil)))
  name)

(defun symbol-macro-modified-p (kernel name expansion)
  (not (and (kernel-symbol-macro-exists-p kernel name)
            (equal expansion (kernel-symbol-macro-expansion kernel name)))))

(defun kernel-manager-define-memory (manager name qualifiers
                                     &optional expression)
  (unless (not (kernel-manager-module-handle manager))
    (error "The kernel manager has already loaded the kernel module."))
  (symbol-macrolet ((module-path (kernel-manager-module-path manager))
                    (kernel (kernel-manager-kernel manager)))
    (when (memory-modified-p kernel name qualifiers expression)
      (kernel-define-memory kernel name qualifiers expression)
      (setf module-path nil)))
  name)

(defun memory-modified-p (kernel name qualifiers expression)
  (not (and (kernel-memory-exists-p kernel name)
            (equal (ensure-list qualifiers)
                   (kernel-address-space-qualifiers kernel name))
            (equal expression (kernel-memory-expression kernel name)))))

(defun kernel-manager-translate (manager)
  (unless (not (kernel-manager-compiled-p manager))
    (error "The kernel manager has already been compiled."))
  (let ((kernel (kernel-manager-kernel manager)))
    (compile-kernel kernel)))

(defun kernel-manager-function-c-name (manager name)
  (kernel-function-c-name (kernel-manager-kernel manager)
                          name))

(defun ensure-kernel-module-compiled (manager)
  (or (kernel-manager-compiled-p manager)
      (kernel-manager-compile-module manager)))

(defun ensure-kernel-module-loaded (manager)
  (ensure-kernel-module-compiled manager)
  (or (kernel-manager-module-handle manager)
      (kernel-manager-load-module manager)))

(defun ensure-kernel-function-loaded (manager name)
  (ensure-kernel-module-loaded manager)
  (or (kernel-manager-function-handle manager name)
      (kernel-manager-load-function manager name)))

(defun ensure-kernel-global-loaded (manager name)
  (ensure-kernel-module-loaded manager)
  (or (kernel-manager-memory-object manager name)
      (kernel-manager-load-global manager name)))

(defun expand-macro-1 (form manager)
  (let ((kernel (kernel-manager-kernel manager)))
    (oclcl.lang.kernel:expand-macro-1 form kernel)))

(defun expand-macro (form manager)
  (let ((kernel (kernel-manager-kernel manager)))
    (oclcl.lang.kernel:expand-macro form kernel)))

(defvar *kernel-manager*)

