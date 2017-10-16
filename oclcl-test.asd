#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2017 gos-k (mag4.elan@gmail.com)
|#

(defsystem "oclcl-test"
  :author "gos-k"
  :license "LLGPL"
  :depends-on ("oclcl"
               "prove"
               "arrow-macros")
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
                   (:test-file "program")
                   (:test-file "compiler/compile-data")
                   (:test-file "compiler/compile-type")
                   (:test-file "compiler/type-of-expression")
                   (:test-file "compiler/compile-expression")
                   (:test-file "compiler/compile-statement")
                   (:test-file "compiler/compile-program")))
                 (:module "api"
                  :serial t
                  :components
                  ((:test-file "defkernel"))))))
  :defsystem-depends-on ("prove-asdf")
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
