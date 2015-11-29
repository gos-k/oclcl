#|
  This file is a part of cl-cuda project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-cuda-test.api.kernel-manager
  (:use :cl :cl-test-more
        :cl-cuda.lang
        :cl-cuda.api.kernel-manager))
(in-package :cl-cuda-test.api.kernel-manager)

(plan nil)


;;;
;;; test KERNEL-MANAGER's state transfer
;;;

(diag "KERNEL-MANAGER")

(let* ((mgr (make-kernel-manager))
       (*kernel-manager* mgr))
    ;; I - initial state
    (kernel-manager-define-function mgr 'foo 'void '() '())
    (is (kernel-manager-compiled-p mgr) nil
        "basic case 1")
    (is (kernel-manager-module-handle mgr) nil
        "basic case 2")
    (is (kernel-manager-function-handles-empty-p mgr) t
        "basic case 3")
    ;; I - initial state
    (kernel-manager-define-function mgr 'bar 'void '() '())
    (is (kernel-manager-compiled-p mgr) nil
        "basic case 16")
    (is (kernel-manager-module-handle mgr) nil
        "basic case 17")
    (is (kernel-manager-function-handles-empty-p mgr) t
        "basic case 18"))

(diag "KERNEL-MANAGER-TRANSLATE")

(let* ((mgr (make-kernel-manager))
       (*kernel-manager* mgr))
  (kernel-manager-define-function mgr
                                  'one
                                  'float
                                  '()
                                  '((return (+ 1 2 3 4 5 6 7))))
  (is (kernel-manager-translate mgr)
"#include \"int.h\"
#include \"float.h\"
#include \"float3.h\"
#include \"float4.h\"
#include \"double.h\"
#include \"double3.h\"
#include \"double4.h\"
#include \"curand.h\"


/**
 *  Kernel function prototypes
 */

 float cl_cuda_test_api_kernel_manager_one();


/**
 *  Kernel function definitions
 */

 float cl_cuda_test_api_kernel_manager_one()
{
  return (1 + (2 + (3 + (4 + (5 + (6 + 7))))));
}
"
      ))

;;;
;;; test KERNEL-MANAGER-DEFINE-FUNCTION function
;;;

(diag "KERNEL-MANAGER-DEFINE-FUNCTION")

(let* ((mgr (make-kernel-manager))
       (*kernel-manager* mgr))
    ;; defining function without change makes no state transfer
    (kernel-manager-define-function mgr 'foo 'void '() '())
    (is (kernel-manager-compiled-p mgr) t
        "basic case 2")
    ;; defining function with change makes state transfer
    (kernel-manager-define-function mgr 'foo 'int '((i int)) '(return i))
    (is (kernel-manager-compiled-p mgr) nil
        "basic case 3"))

(let* ((mgr (make-kernel-manager))
       (*kernel-manager* mgr))
    ;; I - initial state
    (kernel-manager-define-function mgr 'foo 'void '() '())
    nil)

;;;
;;; test KERNEL-MANAGER-DEFINE-MACRO function
;;;

(diag "KERNEL-MANAGER-DEFINE-MACRO")
(let* ((mgr (make-kernel-manager))
       (*kernel-manager* mgr))
    ;; defining macro with change makes state transfer
    (kernel-manager-define-macro mgr 'foo '(a) '(a))
    (is (kernel-manager-compiled-p mgr) nil
        "basic case 3"))

(let* ((mgr (make-kernel-manager))
       (*kernel-manager* mgr))
    ;; I - initial state
    (kernel-manager-define-function mgr 'foo 'void '() '())
    nil)

;;;
;;; test KERNEL-MANAGER-DEFINE-SYMBOL-MACRO function
;;;

(diag "KERNEL-MANAGER-DEFINE-SYMBOL-MACRO")

(let* ((mgr (make-kernel-manager))
       (*kernel-manager* mgr))
    ;; transfer state from I to II
    (kernel-manager-define-symbol-macro mgr 'foo 1)
    ;; defining macro without change makes no state transfer
    (kernel-manager-define-symbol-macro mgr 'foo 1)
    (is (kernel-manager-compiled-p mgr) t
        "basic case 2")
    ;; defining macro with change makes state transfer
    (kernel-manager-define-symbol-macro mgr 'foo 2)
    (is (kernel-manager-compiled-p mgr) nil
        "basic case 3"))

(let* ((mgr (make-kernel-manager))
       (*kernel-manager* mgr))
    ;; I - initial state
    (kernel-manager-define-function mgr 'foo 'void '() '())
    nil)

;;;
;;; test EXPAND-MACRO-1 function
;;;

(diag "EXPAND-MACRO-1")

(let ((mgr (make-kernel-manager)))
  (kernel-manager-define-macro mgr 'foo '(x) '(`(return ,x)))
  (kernel-manager-define-macro mgr 'bar '(x) '(`(foo ,x)))
  (kernel-manager-define-symbol-macro mgr 'a 1.0)
  (kernel-manager-define-symbol-macro mgr 'b 'a)
  (is-values (expand-macro-1 '(foo 1) mgr) '((return 1) t))
  (is-values (expand-macro-1 '(bar 1) mgr) '((foo 1) t))
  (is-values (expand-macro-1 '(baz 1) mgr) '((baz 1) nil))
  (is-values (expand-macro-1 'a mgr) '(1.0 t))
  (is-values (expand-macro-1 'b mgr) '(a t))
  (is-values (expand-macro-1 'c mgr) '(c nil))
  (is-error (expand-macro-1 '(foo) mgr) error))


;;;
;;; test EXPAND-MACRO function
;;;

(diag "EXPAND-MACRO")

(let ((mgr (make-kernel-manager)))
  (kernel-manager-define-macro mgr 'foo '(x) '(`(return ,x)))
  (kernel-manager-define-macro mgr 'bar '(x) '(`(foo ,x)))
  (kernel-manager-define-symbol-macro mgr 'a 1.0)
  (kernel-manager-define-symbol-macro mgr 'b 'a)
  (is-values (expand-macro '(foo 1) mgr) '((return 1) t))
  (is-values (expand-macro '(bar 1) mgr) '((return 1) t))
  (is-values (expand-macro '(baz 1) mgr) '((baz 1) nil))
  (is-values (expand-macro 'a mgr) '(1.0 t))
  (is-values (expand-macro 'b mgr) '(1.0 t))
  (is-values (expand-macro 'c mgr) '(c nil))
  (is-error (expand-macro '(foo) mgr) error))


(finalize)
