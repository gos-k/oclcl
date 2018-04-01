#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2018 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.built-in
  (:use :cl :prove
        :oclcl.lang.type
        :oclcl.lang.data
        :oclcl.lang.built-in))
(in-package :oclcl-test.lang.built-in)

(plan nil)


;;;
;;; test BUILT-IN-FUNCTION-RETURN-TYPE function
;;;

(subtest "BUILT-IN-FUNCTION-RETURN-TYPE"
  (loop for (opes args ret) in '(((+ * - / mod) (int int) int)
                                 ((+ * - / mod) (int int) int)
                                 ((+ * - /) (float float) float)
                                 ((+ * - / mod) (int4 int4) int4)
                                 ((+ * - /) (float4 float4) float4)
                                 ((< > <= >=) (int int) bool)
                                 ((< > <= >=) (float4 float4) int4)
                                 ((< > <= >=) (double4 double4) long4)
                                 ((fmax fmin) (float4 float4) float4)
                                 ((fmax fmin) (float4 float) float4)
                                 ((fmax fmin) (double4 double4) double4)
                                 ((fmax fmin) (double4 double) double4))
        do (dolist (ope opes)
             (is (built-in-function-return-type ope args) ret)))

  (loop for (opes args rets) in '(((nan) ((uint4) (ulong4))
                                         (float4 double4)))
        do (dolist (ope opes)
             (mapcar #'(lambda (arg ret)
                         (is (built-in-function-return-type ope arg) ret))
                     args
                     rets))))


;;;
;;; test BUILT-IN-FUNCTION-INFIX-P function
;;;

(subtest "BUILT-IN-FUNCTION-INFIX-P"
  (loop for (ope args) in '((+ (int int))
                            (+ (float3 float3))
                            (- (int int))
                            (mod (int int)))
        do (ok (built-in-function-infix-p ope args))))

;;;
;;; test BUILT-IN-FUNCTION-C-NAME function
;;;

(subtest "BUILT-IN-FUNCTION-C-NAME"
  (loop for (ope args name) in '((+ (int int) "+")
                                 (+ (float3 float3) "+")
                                 (- (int int) "-")
                                 (mod (int int) "%")
                                 (printf (string) "printf")
                                 (printf (string int) "printf"))
        do (is (built-in-function-c-name ope args) name)))

(finalize)
