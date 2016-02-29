#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.compiler.compile-data
  (:use :cl :prove
        :oclcl.lang.compiler.compile-data))
(in-package :oclcl-test.lang.compiler.compile-data)

(plan nil)


;;;
;;; test COMPILE-SYMBOL function
;;;

(subtest "COMPILE-SYMBOL"
  (is (compile-symbol 'x) "x"
      "basic case 1")
  (is (compile-symbol 'vec-add-kernel) "vec_add_kernel"
      "basic case 2"))


;;;
;;; test COMPILE-BOOL function
;;;

(subtest "COMPILE-BOOL"
  (is (compile-bool t) "true"
      "basic case 1")
  (is (compile-bool nil) "false"
      "basic case 2"))


;;;
;;; test COMPILE-INT function
;;;

(subtest "COMPILE-INT"
  (is (compile-int 1) "1"
      "basic case 1"))


;;;
;;; test COMPILE-FLOAT function
;;;

(subtest "COMPILE-FLOAT"
  (is (compile-float 1.0) "1.0f")
  (is (compile-float 1.23456789012345) "1.2345679f"))


;;;
;;; test COMPILE-DOUBLE function
;;;

(subtest "COMPILE-DOUBLE"
  (is (compile-double 1.0d0) "1.0")
  (is (compile-double 1.23456789012345d0) "1.23456789012345"))

;;; test COMPILE-STRING function

(subtest "compile-string"
  (is (compile-string "unittest compile-string")
      "\"unittest compile-string\""))

(finalize)
