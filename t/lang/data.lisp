#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.data
  (:use :cl :cl-test-more
        :oclcl.lang.data
        :oclcl.lang.type))
(in-package :oclcl-test.lang.data)

(plan nil)


;;;
;;; test Float3
;;;

(diag "Float3")

(let ((cffi-type (cffi-type 'float3)))
  (cffi:with-foreign-object (x cffi-type)
  (setf (cffi:mem-ref x cffi-type) (make-float3 1.0 1.0 1.0))
  (is (cffi:mem-ref x cffi-type) (make-float3 1.0 1.0 1.0)
      :test #'float3-=
      "basic case 1")))


(finalize)
