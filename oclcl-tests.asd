#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2017-2025 gos-k (mag4.elan@gmail.com)
|#

(defsystem "oclcl-tests"
  :author "gos-k"
  :license "LLGPL"
  :depends-on ("oclcl"
               "rove"
               "arrow-macros")
  :pathname "tests"
  :components ((:file "utils")
               (:module "lang"
                :components
                ((:file "util")
                 (:file "data")
                 (:file "type")
                 (:file "syntax")
                 (:file "environment")
                 (:file "built-in")
                 (:file "program")
                 (:module "compiler"
                  :components
                  ((:file "compile-data")
                   (:file "compile-type")
                   (:file "type-of-expression")
                   (:file "compile-expression")
                   (:file "compile-statement")
                   (:file "compile-program")))))
               (:module "api"
                :components
                ((:file "defkernel"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c :style :dot)))
