#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.compiler.compile-kernel
  (:use :cl
        :oclcl.lang.util
        :oclcl.lang.type
        :oclcl.lang.syntax
        :oclcl.lang.environment
        :oclcl.lang.kernel
        :oclcl.lang.compiler.compile-data
        :oclcl.lang.compiler.compile-type
        :oclcl.lang.compiler.compile-expression
        :oclcl.lang.compiler.compile-statement
        :oclcl.lang.compiler.type-of-expression)
  (:export :compile-kernel))
(in-package :oclcl.lang.compiler.compile-kernel)


;;;
;;; Kernel to Environment
;;;

(defun %add-function-arguments (kernel name var-env)
  (flet ((aux (var-env0 argument)
           (let ((var (argument-var argument))
                 (type (argument-type argument)))
             (variable-environment-add-variable var type var-env0))))
    (reduce #'aux (kernel-function-arguments kernel name)
            :initial-value var-env)))

(defun %add-symbol-macros (kernel var-env)
  (flet ((aux (var-env0 name)
           (let ((expansion (kernel-symbol-macro-expansion kernel name)))
             (variable-environment-add-symbol-macro name expansion
                                                    var-env0))))
    (reduce #'aux (kernel-symbol-macro-names kernel)
            :initial-value var-env)))

(defun %add-globals (kernel var-env)
  (flet ((aux (var-env0 name)
           (let* ((expression (kernel-global-expression kernel name))
                  (type (type-of-expression expression nil nil)))
             (variable-environment-add-global name type expression var-env0))))
    (reduce #'aux (kernel-global-names kernel)
            :initial-value var-env)))

(defun kernel->variable-environment (kernel name)
  (if name
      (%add-function-arguments kernel name
                               (%add-symbol-macros kernel
                                                   (%add-globals kernel
                                                                 (empty-variable-environment))))
      (%add-symbol-macros kernel
                          (%add-globals kernel
                                        (empty-variable-environment)))))

(defun %add-functions (kernel func-env)
  (flet ((aux (func-env0 name)
           (let ((return-type (kernel-function-return-type kernel name))
                 (argument-types (kernel-function-argument-types kernel
                                                                 name)))
             (function-environment-add-function name return-type
                                               argument-types func-env0))))
    (reduce #'aux (kernel-function-names kernel)
            :initial-value func-env)))

(defun %add-macros (kernel func-env)
  (flet ((aux (func-env0 name)
           (let ((arguments (kernel-macro-arguments kernel name))
                 (body (kernel-macro-body kernel name)))
             (function-environment-add-macro name arguments body func-env0))))
    (reduce #'aux (kernel-macro-names kernel)
            :initial-value func-env)))

(defun kernel->function-environment (kernel)
  (%add-functions kernel
    (%add-macros kernel
      (empty-function-environment))))

;;;
;;; Compile kernel
;;;

(defun compile-includes ()
  "")

(defun compile-function-qualifier (return-type)
  (unless (oclcl-type-p return-type)
    (error 'type-error :datum return-type :expected 'oclcl-type))
  ;; OpenCL v1.2 dr19: 6.7 Function Qualifiers
  (if (eq return-type 'void)
      "__kernel"
      nil))

(defun compile-address-space-qualifier (qualifier)
  (format nil "__~A" (string-downcase (princ-to-string qualifier))))

(defun compile-global (kernel name)
  (let ((c-name (kernel-global-c-name kernel name))
        (qualifiers (kernel-global-qualifiers kernel name))
        (expression (kernel-global-expression kernel name)))
    (let ((type1 (compile-type
                  (type-of-expression expression nil nil)))
          (qualifiers1 (mapcar #'compile-address-space-qualifier qualifiers))
          (expression1 (compile-expression expression
                        (kernel->variable-environment kernel nil)
                        (kernel->function-environment kernel))))
      (format nil "~{~A~^ ~} ~A ~A~@[ = ~A~];~%"
              qualifiers1 type1 c-name expression1))))

(defun compile-globals (kernel)
  (flet ((aux (name)
           (compile-global kernel name)))
    (let ((globals (mapcar #'aux (kernel-global-names kernel))))
      (if (null globals)
          ""
          (format nil "/**
 *  Memory objects
 */

~{~A~}" globals)))))

(defun compile-argument (argument)
  (let ((var (argument-var argument))
        (type (argument-type argument)))
    (let ((var1 (compile-symbol var))
          (type1 (compile-type type)))
      (format nil "~A ~A" type1 var1))))

(defun compile-arguments (arguments)
  (let ((arguments1 (mapcar #'compile-argument arguments)))
    (if arguments1
        (format nil " ~{~A~^, ~} " arguments1)
        "")))

(defun compile-declaration (kernel name)
  (let ((c-name (kernel-function-c-name kernel name))
        (return-type (kernel-function-return-type kernel name))
        (arguments (kernel-function-arguments kernel name)))
    (let ((function-qualifier (compile-function-qualifier return-type))
          (return-type1 (compile-type return-type))
          (arguments1 (compile-arguments arguments)))
      (if function-qualifier
          (format nil "~A ~A ~A(~A)" function-qualifier return-type1 c-name arguments1)
          (format nil "~A ~A(~A)" return-type1 c-name arguments1)))))

(defun compile-prototype (kernel name)
  (let ((declaration (compile-declaration kernel name)))
    (format nil "~A;~%" declaration)))

(defun compile-prototypes (kernel)
  (flet ((aux (name)
           (compile-prototype kernel name)))
    (let ((prototypes (mapcar #'aux (kernel-function-names kernel))))
      (if (null prototypes)
          ""
          (format nil "/**
 *  Kernel function prototypes
 */

~{~A~}" prototypes)))))

(defun compile-statements (kernel name)
  (let ((var-env (kernel->variable-environment kernel name))
        (func-env (kernel->function-environment kernel)))
    (flet ((aux (statement)
             (compile-statement statement var-env func-env)))
      (let ((statements (kernel-function-body kernel name)))
        (format nil "~{~A~}" (mapcar #'aux statements))))))

(defun compile-definition (kernel name)
  (let ((declaration (compile-declaration kernel name))
        (statements (compile-statements kernel name)))
    (let ((statements1 (indent 2 statements)))
      (format nil "~A~%{~%~A}~%" declaration statements1))))

(defun compile-definitions (kernel)
  (flet ((aux (name)
           (compile-definition kernel name)))
    (let ((definitions (mapcar #'aux (kernel-function-names kernel))))
      (if (null definitions)
          ""
          (format nil "/**
 *  Kernel function definitions
 */

~{~A~^~%~}" definitions)))))

(defun compile-kernel (kernel)
  (let ((includes (compile-includes))
        (globals (compile-globals kernel))
        (prototypes (compile-prototypes kernel))
        (definitions (compile-definitions kernel)))
    (format nil "~A~%~%~A~%~%~A~%~%~A" includes
                                       globals
                                       prototypes
                                       definitions)))
