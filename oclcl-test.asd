#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test-asd
  (:use :cl :asdf))
(in-package :oclcl-test-asd)

(defsystem oclcl-test
  :author "gos-k"
  :license "LLGPL"
  :depends-on (:oclcl
               :prove
               :arrow-macros)
  :components ((:module "t"
                :serial t
                :components
                ((:module "lang"
                  :serial t
                  :components
                  ((:test-file "util")
                   (:test-file "data")
                   (:test-file "type")
                   (:test-file "syntax")
                   (:test-file "environment")
                   (:test-file "built-in")
                   (:test-file "kernel")
                   (:test-file "compiler/compile-data")
                   (:test-file "compiler/compile-type")
                   (:test-file "compiler/type-of-expression")
                   (:test-file "compiler/compile-expression")
                   (:test-file "compiler/compile-statement")
                   (:test-file "compiler/compile-kernel")))
                 (:module "api"
                  :serial t
                  :components
                  ((:test-file "defkernel")
                   (:test-file "kernel-manager"))))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf) c)
                    (asdf:clear-system c)))
