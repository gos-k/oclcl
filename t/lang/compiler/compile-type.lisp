#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.compiler.compile-type
  (:use :cl :cl-test-more
        :oclcl.lang.type
        :oclcl.lang.compiler.compile-type))
(in-package :oclcl-test.lang.compiler.compile-type)

(plan nil)


;;;
;;; test COMPILE-TYPE function
;;;

(diag "COMPILE-TYPE")

(is (compile-type 'int) "int"
    "basic case 1")



(finalize)
