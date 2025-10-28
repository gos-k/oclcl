#|
  This file is a part of oclcl project.
  Copyright (c) 2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage :oclcl.tests.utils
  (:use :cl :rove)
  (:import-from #:oclcl.lang.environment
                #:empty-environment)
  (:export #:is
           #:is-values
           #:with-empty-env))
(in-package :oclcl.tests.utils)

(defmacro is (lhs rhs &optional desc)
  `(ok (equal ,lhs ,rhs) ,desc))

(defmacro is-values (lhs rhs &optional desc)
  `(ok (equalp (multiple-value-list ,lhs) ,rhs) ,desc))

(defun call-with-empty-env (fun)
  (funcall fun (empty-environment)))

(defmacro with-empty-env ((env) &body body)
  `(call-with-empty-env (lambda (,env) ,@body)))