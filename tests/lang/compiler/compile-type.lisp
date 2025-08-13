#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.tests.lang.compiler.compile-type
  (:use :cl :rove
        :oclcl.tests.utils
        :oclcl.lang.type
        :oclcl.lang.compiler.compile-type))
(in-package :oclcl.tests.lang.compiler.compile-type)

;;;
;;; test COMPILE-TYPE function
;;;

(deftest compile-type
  (is (compile-type 'int) "int"
      "basic case 1"))


