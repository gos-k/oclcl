#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage :oclcl.api.macro
  (:use :cl
        :oclcl.api.defkernel)
  (:export :let*
           :when
           :unless))
(in-package :oclcl.api.macro)


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
