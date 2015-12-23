#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.api
  (:use :cl :cl-reexport))
(in-package :oclcl.api)

(reexport-from :oclcl.api.defkernel)
(reexport-from :oclcl.api.macro)

;; reexport no symbols from oclcl.api.kernel-manager package
