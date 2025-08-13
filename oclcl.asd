#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2017-2025 gos-k (mag4.elan@gmail.com)
|#

;;;
;;; oclcl system definition
;;;

(defsystem "oclcl"
  :version "0.1"
  :author "gos-k"
  :license "LLGPL"
  :depends-on ("cffi" "alexandria" "external-program" "osicat"
                      "cl-pattern" "split-sequence" "cl-reexport" "cl-ppcre"
                      "lisp-namespace")
  :components ((:module "src"
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
                   (:file "program")
                   (:file "compiler/compile-data")
                   (:file "compiler/compile-type")
                   (:file "compiler/type-of-expression")
                   (:file "compiler/compile-expression")
                   (:file "compiler/compile-statement")
                   (:file "compiler/compile-program")
                   (:file "lang")))
                 (:module "api"
                  :serial t
                  :components
                  ((:file "defkernel")
                   (:file "epilogue")
                   (:file "api")))
                 (:file "oclcl"))))
  :description "oclcl is a library S-expression to OpenCL C."
  ;; :long-description #.(read-file-string (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "oclcl-tests"))))
