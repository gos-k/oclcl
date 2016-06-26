#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-examples-asd
  (:use :cl :asdf))
(in-package :oclcl-examples-asd)

(defsystem oclcl-examples
  :author "gos-k"
  :license "LLGPL"
  :depends-on (:oclcl
               ;:eazy-opencl
               :cl-oclapi
               :imago)
  :components ((:module "examples"
                :components
                ((:file "sph-cpu")
                 ;(:file "vector-add")
                 ;(:file "diffuse0")
                 ;(:file "diffuse1")
                 (:file "vector-add-oclapi")
                 (:file "diffuse0-oclapi")
                 (:file "diffuse1-oclapi")
                 (:file "sph-oclapi"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf) c)
                    (asdf:clear-system c)))
