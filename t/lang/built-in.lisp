#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.built-in
  (:use :cl :prove
        :oclcl.lang.type
        :oclcl.lang.built-in))
(in-package :oclcl-test.lang.built-in)

(plan nil)


;;;
;;; test BUILT-IN-FUNCTION-RETURN-TYPE function
;;;

(diag "BUILT-IN-FUNCTION-RETURN-TYPE")

(dolist (operator '(+ * - / mod))
  (is (built-in-function-return-type operator '(int int)) 'int))
(dolist (operator '(+ * - /))
  (is (built-in-function-return-type operator '(float float)) 'float))
(dolist (operator '(+ * - / mod))
  (is (built-in-function-return-type operator '(int4 int4)) 'int4))
(dolist (operator '(+ * - /))
  (is (built-in-function-return-type operator '(float4 float4)) 'float4))
(dolist (operator '(< > <= >=))
  (is (built-in-function-return-type operator '(int int)) 'int))
(dolist (operator '(< > <= >=))
  (is (built-in-function-return-type operator '(float4 float4)) 'int4))
(dolist (operator '(< > <= >=))
  (is (built-in-function-return-type operator '(double4 double4)) 'long4))

;;;
;;; test BUILT-IN-FUNCTION-INFIX-P function
;;;

(diag "BUILT-IN-FUNCTION-INFIX-P")

(is (built-in-function-infix-p '+ '(int int)) t
    "basic case 1")

(is (built-in-function-infix-p '+ '(float3 float3)) t
    "basic case 2")

(is (built-in-function-infix-p '- '(int int)) t
    "basic case 3")

(is (built-in-function-infix-p 'mod '(int int)) t
    "basic case 4")

;;;
;;; test BUILT-IN-FUNCTION-C-NAME function
;;;

(diag "BUILT-IN-FUNCTION-C-NAME")

(is (built-in-function-c-name '+ '(int int)) "+"
    "basic case 1")

(is (built-in-function-c-name '+ '(float3 float3)) "+"
    "basic case 2")

(is (built-in-function-c-name '- '(int int)) "-"
    "basic case 3")

(is (built-in-function-c-name 'mod '(int int)) "%"
    "basic case 4")


(finalize)
