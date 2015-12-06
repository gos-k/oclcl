#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
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
           :kernel-manager-define-function
           :kernel-manager-define-macro
           :kernel-manager-define-symbol-macro
           :kernel-manager-translate
           :kernel-manager-unload
           :ensure-kernel-module-compiled
           :ensure-kernel-module-loaded
           :ensure-kernel-function-loaded
           :expand-macro-1
           :expand-macro
           :*kernel-manager*)
  (:shadow :expand-macro-1
           :expand-macro))
(in-package :oclcl.api.kernel-manager)


;;;
;;; Kernel manager
;;;

(defstruct (kernel-manager (:constructor %make-kernel-manager))
  module-path
  module-handle
  %function-handles
  kernel)

(defun make-kernel-manager ()
  (%make-kernel-manager :module-path nil
                        :module-handle nil
                        :%function-handles (make-hash-table)
                        :kernel (make-kernel)))

(defun kernel-manager-%function-handle (manager name)
  (let ((function-handles (kernel-manager-%function-handles manager)))
    (gethash name function-handles)))

(defun (setf kernel-manager-%function-handle) (value manager name)
  (let ((function-handles (kernel-manager-%function-handles manager)))
    (setf (gethash name function-handles) value)))

(defun kernel-manager-compiled-p (manager)
  (and (kernel-manager-module-path manager)
       t))

(defun kernel-manager-function-handles-empty-p (manager)
  (let ((function-handles (kernel-manager-%function-handles manager)))
    (= (hash-table-count function-handles) 0)))

(defun kernel-manager-function-handle (manager name)
  (kernel-manager-%function-handle manager name))

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

(defun kernel-manager-translate (manager)
  (unless (not (kernel-manager-compiled-p manager))
    (error "The kernel manager has already been compiled."))
  (let ((kernel (kernel-manager-kernel manager)))
    (compile-kernel kernel)))

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

(defun expand-macro-1 (form manager)
  (let ((kernel (kernel-manager-kernel manager)))
    (oclcl.lang.kernel:expand-macro-1 form kernel)))

(defun expand-macro (form manager)
  (let ((kernel (kernel-manager-kernel manager)))
    (oclcl.lang.kernel:expand-macro form kernel)))

(defvar *kernel-manager* (make-kernel-manager))
