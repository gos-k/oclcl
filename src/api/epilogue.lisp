#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage :oclcl.api.epilogue
  (:use :cl
        :oclcl.api.defkernel
        :oclcl.lang.program)
  (:export :let*
           :when
           :unless)
  (:documentation
   "Contains the epilogue code for the system that defines the default set of
   kernel module and its kernel macros."))
(in-package :oclcl.api.epilogue)

(define-program :oclcl)
(in-program :oclcl)

(defkernelmacro let* (bindings &body body)
  (if bindings
      `(let (,(car bindings))
         (let* (,@(cdr bindings))
           ,@body))
      `(progn ,@body)))

(defkernelmacro when (test &body body)
  `(if ,test
       (progn ,@body)))

(defkernelmacro unless (test &body body)
  `(if (not ,test)
       (progn ,@body)))
