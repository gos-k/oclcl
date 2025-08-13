#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.tests.lang.compiler.compile-data
  (:use :cl :rove
        :oclcl.tests.utils
        :oclcl.lang.compiler.compile-data))
(in-package :oclcl.tests.lang.compiler.compile-data)

;;;
;;; test COMPILE-SYMBOL function
;;;

(deftest compile-symbol
  (is (compile-symbol 'x) "x"
      "basic case 1")
  (is (compile-symbol 'vec-add-kernel) "vec_add_kernel"
      "basic case 2"))


;;;
;;; test COMPILE-BOOL function
;;;

(deftest compile-bool
  (is (compile-bool t) "true"
      "basic case 1")
  (is (compile-bool nil) "false"
      "basic case 2"))


;;;
;;; test COMPILE-INT function
;;;

(deftest compile-int
  (is (compile-int 1) "1"
      "basic case 1"))


;;;
;;; test COMPILE-FLOAT function
;;;

(deftest compile-float
  (is (compile-float 1.0f0) "1.0f")
  (is (compile-float 1.23456789012345f0) "1.2345679f"))


;;;
;;; test COMPILE-DOUBLE function
;;;

(deftest compile-double
  (is (compile-double 1.0d0) "1.0")
  (is (compile-double 1.23456789012345d0) "1.23456789012345"))

;;; test COMPILE-STRING function

(deftest compile-string
  (is (compile-string "unittest compile-string")
      "\"unittest compile-string\""))
