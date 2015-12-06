#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-examples-asd
  (:use :cl :asdf))
(in-package :oclcl-examples-asd)

(defsystem oclcl-examples
  :author "gos-k"
  :license "LLGPL"
  :depends-on (:oclcl
               :prove
               :imago)
  :components ((:module "examples"
                :components
                ((:file "diffuse0")
                 (:file "diffuse1")
                 ; (:file "shared-memory")
                 (:file "vector-add")
                 (:file "sph"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf) c)
                    (asdf:clear-system c)))
