#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-asd
  (:use :cl :asdf))
(in-package :oclcl-asd)


;;;
;;; oclcl system definition
;;;

(defsystem oclcl
  :version "0.1"
  :author "gos-k"
  :license "LLGPL"
  :depends-on (:cffi :alexandria :external-program :osicat
               :cl-pattern :split-sequence :cl-reexport :cl-ppcre
               :arrow-macros)
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
                   (:file "kernel")
                   (:file "compiler/compile-data")
                   (:file "compiler/compile-type")
                   (:file "compiler/type-of-expression")
                   (:file "compiler/compile-expression")
                   (:file "compiler/compile-statement")
                   (:file "compiler/compile-kernel")
                   (:file "lang")))
                 (:module "api"
                  :serial t
                  :components
                  ((:file "kernel-manager")
                   (:file "defkernel")
                   (:file "macro")
                   (:file "api")))
                 (:file "oclcl"))))
  :description "oclcl is a library S-expression to OpenCL C."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op oclcl-test))))
