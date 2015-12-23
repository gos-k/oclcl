#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.compiler.compile-data
  (:use :cl
        :oclcl.lang.data
        :oclcl.lang.util)
  (:export :compile-symbol
           :compile-bool
           :compile-int
           :compile-float
           :compile-double))
(in-package :oclcl.lang.compiler.compile-data)


;;;
;;; Symbol
;;;

(defun compile-symbol (expr)
  (unless (oclcl-symbol-p expr)
    (error "The value ~S is an invalid expression." expr))
  (c-identifier expr))


;;;
;;; Bool
;;;

(defun compile-bool (expr)
  (unless (oclcl-bool-p expr)
    (error "The value ~S is an invalid expression." expr))
  (if expr "true" "false"))


;;;
;;; Int
;;;

(defun compile-int (expr)
  (unless (oclcl-int-p expr)
    (error "The value ~S is an invalid expression." expr))
  (princ-to-string expr))


;;;
;;; Float
;;;

(defun compile-float (expr)
  (unless (oclcl-float-p expr)
    (error "The value ~S is an invalid expression." expr))
  (format nil "~Ff" expr))


;;;
;;; Double
;;;

(defun compile-double (expr)
  (unless (oclcl-double-p expr)
    (error "The value ~S is an invalid expression." expr))
  (format nil "~F" (float expr 0.0d0)))
