#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test-asd
  (:use :cl :asdf))
(in-package :oclcl-test-asd)

(defsystem oclcl-test
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:oclcl
               :cl-test-more)
  :components ((:module "t"
                :serial t
                :components
                ((:module "lang"
                  :serial t
                  :components
                  ((:file "util")
                   (:file "data")
                   (:file "type")
                   (:file "syntax")
                   (:file "environment")
                   (:file "built-in")
                   (:file "kernel")
                   (:file "compiler/compile-data")
                   (:file "compiler/compile-type")
                   (:file "compiler/type-of-expression")
                   (:file "compiler/compile-expression")
                   (:file "compiler/compile-statement")
                   (:file "compiler/compile-kernel")))
                 (:module "api"
                  :serial t
                  :components
                  ((:file "defkernel")
                   (:file "kernel-manager"))))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
