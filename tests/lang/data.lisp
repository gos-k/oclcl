#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.tests.lang.data
  (:use :cl :rove
        :oclcl.tests.utils
        :oclcl.lang.data
        :oclcl.lang.type))
(in-package :oclcl.tests.lang.data)

;;;
;;; test Float3
;;;

(deftest float3
  (let ((cffi-type (cffi-type 'float3)))
    (cffi:with-foreign-object (x cffi-type)
      (setf (cffi:mem-ref x cffi-type) (make-float3 1.0f0 1.0f0 1.0f0))
      (ok (float3-= (cffi:mem-ref x cffi-type) (make-float3 1.0f0 1.0f0 1.0f0))
          "basic case 1"))))

