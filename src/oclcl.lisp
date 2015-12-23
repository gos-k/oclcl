#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl
  (:use :cl :cl-reexport))
(in-package :oclcl)

(reexport-from :oclcl.lang)
(reexport-from :oclcl.api)
