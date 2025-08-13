#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.tests.lang.util
  (:use :cl :rove
        :oclcl.tests.utils
        :oclcl.lang.util))
(in-package :oclcl.tests.lang.util)

;;;
;;; test C-IDENTIFIER function
;;;

(deftest c-identifier
  (is (c-identifier 'x) "x"
      "basic case 1")
  (is (c-identifier 'vec-add-kernel) "vec_add_kernel"
      "basic case 2")
  (is (c-identifier 'vec.add.kernel) "vec_add_kernel"
      "basic case 3")
  (is (c-identifier '%vec-add-kernel) "_vec_add_kernel"
      "basic case 4")
  (is (c-identifier 'VecAdd_kernel) "vecadd_kernel"
      "basic case 5")
  (is (c-identifier 'foo t) "oclcl_tests_lang_util_foo"
      "basic case 6"))

(deftest c-macro-name
  (is (c-macro-name :--alfa-bravo-charlie--)
      "__ALFA_BRAVO_CHARLIE__"
      "keyword symbol to C macro name"))

;;;
;;; test LINES function
;;;

(deftest lines
  (is (lines (format nil "1~%2~%3~%")) '("1" "2" "3")
      "basic case 1")
  (is (lines (format nil "1~%2~%3")) '("1" "2" "3")
      "basic case 2"))


;
;;; test UNLINES function
;;;

(deftest unlines
  (is (unlines "1" "2" "3") "1
2
3
" "basic case 1"))


;;;
;;; test INDENT function
;;;

(deftest indent
  (is (indent 2 (format nil "1~%2~%3~%")) "  1
  2
  3
" "basic case 1")

  (is (indent 2 (format nil "1~%2~%3")) "  1
  2
  3
" "basic case 2"))
