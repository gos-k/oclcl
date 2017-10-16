#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.api
  (:use :cl :cl-reexport)
  (:documentation "
Exports the symbols for manipulating OpenCL programs.
APIs for writing those programs (e.g. float4, etc...) are not exported from this package."))
(in-package :oclcl.api)

(reexport-from :oclcl.api.defkernel)
(reexport-from :oclcl.api.epilogue)
(reexport-from :oclcl.lang.compiler.compile-program
               :include '(:compile-program))
(reexport-from :oclcl.lang.program)
