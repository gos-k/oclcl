#|
  This file is a part of oclcl project.
  Copyright (c) 2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage :oclcl.tests.utils
  (:use :cl :rove)
  (:export #:is
           #:is-values))
(in-package :oclcl.tests.utils)

(defun is (lhs rhs &optional desc)
  (ok (equal lhs rhs) desc))

(defmacro is-values (lhs rhs &optional desc)
  `(ok (equalp (multiple-value-list ,lhs) ,rhs) ,desc))
