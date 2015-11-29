#|
  This file is a part of cl-cuda project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-cuda.api
  (:use :cl :cl-reexport))
(in-package :cl-cuda.api)

(reexport-from :cl-cuda.api.defkernel)
(reexport-from :cl-cuda.api.macro)

;; reexport no symbols from cl-cuda.api.kernel-manager package
