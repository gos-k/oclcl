#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.compiler.compile-type
  (:use :cl
        :oclcl.lang.type)
  (:export :compile-type))
(in-package :oclcl.lang.compiler.compile-type)


;;;
;;; Type
;;;

(defun compile-type (type)
  (unless (oclcl-type-p type)
    (error "The value ~S is an invalid oclcl type." type))
  (cuda-type type))
