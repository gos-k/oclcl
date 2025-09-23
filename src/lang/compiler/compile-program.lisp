#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.compiler.compile-program
  (:use :cl
        :alexandria
        :oclcl.lang.util
        :oclcl.lang.type
        :oclcl.lang.syntax
        :oclcl.lang.environment
        :oclcl.lang.program
        :oclcl.lang.compiler.compile-data
        :oclcl.lang.compiler.compile-type
        :oclcl.lang.compiler.compile-expression
        :oclcl.lang.compiler.compile-statement
        :oclcl.lang.compiler.type-of-expression)
  (:import-from #:serapeum
                #:fmt)
  (:export :compile-program))
(in-package :oclcl.lang.compiler.compile-program)


;;;
;;; Program to Environment
;;;

(defun %add-function-arguments (program name var-env)
  (flet ((aux (var-env0 argument)
           (let ((var (argument-var argument))
                 (type (argument-type argument)))
             (variable-environment-add-variable var type var-env0))))
    (reduce #'aux (program-function-arguments program name)
            :initial-value var-env)))

(defun %add-symbol-macros (program var-env)
  (flet ((aux (var-env0 name)
           (let ((expansion (program-symbol-macro-expansion program name)))
             (variable-environment-add-symbol-macro name expansion
                                                    var-env0))))
    (reduce #'aux (program-symbol-macro-names program)
            :initial-value var-env)))

(defun %add-memories (program var-env)
  (flet ((aux (var-env0 name)
           (let* ((expression (program-memory-expression program name))
                  (type (type-of-expression expression nil nil)))
             (variable-environment-add-memory name type expression var-env0))))
    (reduce #'aux (program-memory-names program)
            :initial-value var-env)))

(defun program->variable-environment (program name)
  (if name
      (%add-function-arguments program name
                               (%add-symbol-macros program
                                                   (%add-memories program
                                                                 (empty-variable-environment))))
      (%add-symbol-macros program
                          (%add-memories program
                                        (empty-variable-environment)))))

(defun %add-functions (program func-env)
  (flet ((aux (func-env0 name)
           (let ((return-type (program-function-return-type program name))
                 (argument-types (program-function-argument-types program
                                                                 name)))
             (function-environment-add-function name return-type
                                               argument-types func-env0))))
    (reduce #'aux (program-function-names program)
            :initial-value func-env)))

(defun %add-macros (program func-env)
  (flet ((aux (func-env0 name)
           (let ((arguments (program-macro-arguments program name))
                 (body (program-macro-body program name)))
             (function-environment-add-macro name arguments body func-env0))))
    (reduce #'aux (program-macro-names program)
            :initial-value func-env)))

(defun program->function-environment (program)
  (%add-functions program
    (%add-macros program
      (empty-function-environment))))

;;;
;;; Compile program
;;;

(defun compile-includes ()
  nil)

(defun compile-function-qualifier (return-type)
  (unless (oclcl-type-p return-type)
    (error 'type-error :datum return-type :expected 'oclcl-type))
  ;; OpenCL v1.2 dr19: 6.7 Function Qualifiers
  (if (eq return-type 'void)
      "__kernel"
      nil))

(defun compile-address-space-qualifier (qualifier)
  (fmt "__~A" (string-downcase (princ-to-string qualifier))))

(defun compile-memory (program name)
  (let ((c-name (program-memory-c-name program name))
        (qualifiers (program-address-space-qualifiers program name))
        (expression (program-memory-expression program name)))
    (let ((type1 (compile-type
                  (type-of-expression expression nil nil)))
          (qualifiers1 (mapcar #'compile-address-space-qualifier qualifiers))
          (expression1 (compile-expression expression
                        (program->variable-environment program nil)
                        (program->function-environment program))))
      (fmt "~{~A~^ ~} ~A ~A~@[ = ~A~];~%"
              qualifiers1 type1 c-name expression1))))

(defun compile-memories (program)
  (flet ((aux (name)
           (compile-memory program name)))
    (let ((memories (mapcar #'aux (program-memory-names program))))
      (when memories
        (fmt "/**
 *  Memory objects
 */

~{~A~}" memories)))))

(defun compile-argument (argument)
  (let ((var (argument-var argument))
        (type (argument-type argument)))
    (let ((var1 (compile-symbol var))
          (type1 (compile-type type)))
      (fmt "~A ~A" type1 var1))))

(defun compile-arguments (arguments)
  (let ((arguments1 (mapcar #'compile-argument arguments)))
    (if arguments1
        (fmt "~{~A~^, ~}" arguments1)
        "")))

(defun compile-declaration (program name)
  (let ((c-name (program-function-c-name program name))
        (return-type (program-function-return-type program name))
        (arguments (program-function-arguments program name)))
    (let ((function-qualifier (compile-function-qualifier return-type))
          (return-type1 (compile-type return-type))
          (arguments1 (compile-arguments arguments)))
      (if function-qualifier
          (fmt "~A ~A ~A(~A)" function-qualifier return-type1 c-name arguments1)
          (fmt "~A ~A(~A)" return-type1 c-name arguments1)))))

(defun compile-prototype (program name)
  (let ((declaration (compile-declaration program name)))
    (fmt "~A;~%" declaration)))

(defun compile-prototypes (program)
  (flet ((aux (name)
           (compile-prototype program name)))
    (let ((prototypes (mapcar #'aux (reverse (program-function-names program)))))
      (when prototypes
        (fmt "/**
 *  Kernel function prototypes
 */

~{~A~}" prototypes)))))

(defun compile-statements (program name)
  (let ((var-env (program->variable-environment program name))
        (func-env (program->function-environment program)))
    (flet ((aux (statement)
             (compile-statement statement var-env func-env)))
      (let ((statements (program-function-body program name)))
        (fmt "~{~A~}" (mapcar #'aux statements))))))

(defun compile-definition (program name)
  (let ((declaration (compile-declaration program name))
        (statements (compile-statements program name)))
    (let ((statements1 (indent 2 statements)))
      (fmt "~A~%{~%~A}~%" declaration statements1))))

(defun compile-definitions (program)
  (flet ((aux (name)
           (compile-definition program name)))
    (let ((definitions (mapcar #'aux (reverse (program-function-names program)))))
      (when definitions
        (fmt "/**
 *  Kernel function definitions
 */

~{~A~^~%~}" definitions)))))

(defun compile-define (program name)
  (let ((c-name (program-define-c-name program name))
        (expression (program-define-expression program name)))
    (let ((expression1 (compile-expression expression
                                           (program->variable-environment program nil)
                                           (program->function-environment program))))
      (fmt "#define ~A ~A~%" c-name expression1))))

(defun compile-defines (program)
  (let ((defines (mapcar (curry #'compile-define program)
                         (program-define-names program))))
    (when defines
      (fmt "/**
 *  Define
 */

~{~A~}" defines))))

(defun compile-program (program)
  (let* (;(includes (compile-includes))
         (defines (compile-defines program))
         (memories (compile-memories program))
         (prototypes (compile-prototypes program))
         (definitions (compile-definitions program))
         (program (append ;(and includes (list includes))
                          (and defines (list defines))
                          (and memories (list memories))
                          (and prototypes (list prototypes))
                          (and definitions (list definitions)))))
    (fmt "~{~A~^~%~%~}" program)))
