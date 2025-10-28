#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.compiler.compile-statement
  (:use :cl
        :oclcl.lang.util
        :oclcl.lang.type
        :oclcl.lang.syntax
        :oclcl.lang.environment
        :oclcl.lang.compiler.compile-data
        :oclcl.lang.compiler.compile-type
        :oclcl.lang.compiler.type-of-expression
        :oclcl.lang.compiler.compile-expression)
  (:export :compile-statement))
(in-package :oclcl.lang.compiler.compile-statement)


;;;
;;; Compile statement
;;;

(defun compile-statement (form env)
  (cond
    ((%macro-p form env) (compile-macro form env))
    ((if-p form) (compile-if form env))
    ((let-p form) (compile-let form env))
    ((symbol-macrolet-p form) (compile-symbol-macrolet form env))
    ((do-p form) (compile-do form env))
    ((with-local-memory-p form)
     (compile-with-local-memory form env))
    ((set-p form) (compile-set form env))
    ((progn-p form) (compile-progn form env))
    ((return-p form) (compile-return form env))
    ((declare-p form) (compile-declare form env))
    ((function-p form) (compile-function form env))
    (t (error "The value ~S is an invalid statement." form))))


;;;
;;; Macro
;;;

(defun %macro-p (form env)
  (oclcl.lang.compiler.compile-expression::%macro-p form env))

(defun compile-macro (form env)
  (let ((operator (macro-operator form))
        (operands (macro-operands form)))
    (let ((expander (function-environment-macro-expander env operator)))
      (let ((form1 (funcall expander operands)))
        (compile-statement form1 env)))))


;;;
;;; If statement
;;;

(defun compile-if (form env)
  (let ((test-expr (if-test-expression form))
        (then-stmt (if-then-statement form))
        (else-stmt (if-else-statement form)))
    ;; check if the test part of inline-if expression has bool type
    (let ((test-type (type-of-expression test-expr env)))
      (unless (eq test-type 'bool)
        (error "The type of statement ~S is invalid." form)))
    (let ((test-expr1 (compile-expression test-expr env))
          (then-stmt1 (compile-statement then-stmt env))
          (else-stmt1 (if else-stmt
                          (compile-statement else-stmt env))))
      (let ((test-expr2 (if (char/= #\( (char test-expr1 0))
                            (setf test-expr1 (format nil "(~A)" test-expr1))
                            test-expr1))
            (then-stmt2 (indent 2 then-stmt1))
            (else-stmt2 (if else-stmt1
                            (indent 2 else-stmt1))))
        (format nil "if ~A~%{~%~A}~@[~%else~%{~%~A}~]~%" test-expr2 then-stmt2 else-stmt2)))))


;;;
;;; Let statement
;;;

(defun env-add-let-bindings (env bindings)
  (flet ((aux (env0 binding)
           (let* ((var (let-binding-var binding))
                  (expr (let-binding-expr binding))
                  (type (type-of-expression expr env)))
             (variable-environment-add-variable var type env0))))
    (reduce #'aux bindings :initial-value env)))

(defun compile-let-bindings (bindings env)
  (flet ((aux (binding)
           (let* ((var (let-binding-var binding))
                  (expr (let-binding-expr binding))
                  (type (type-of-expression expr env)))
             (let ((var1 (compile-symbol var))
                   (expr1 (compile-expression expr env))
                   (type1 (compile-type type)))
               (format nil "~A ~A = ~A;~%" type1 var1 expr1)))))
    (format nil "~{~A~}" (mapcar #'aux bindings))))

(defun compile-let-statements (statements env)
  (compile-statement `(progn ,@statements) env))

(defun compile-let (form env)
  (let ((bindings (let-bindings form))
        (statements (let-statements form)))
    (let ((env1 (env-add-let-bindings env bindings)))
      (let ((bindings1 (compile-let-bindings bindings env))
            (statements1 (compile-let-statements statements env1)))
        (let ((bindings2 (indent 2 bindings1))
              (statements2 (indent 2 statements1)))
          (format nil "{~%~A~A}~%" bindings2 statements2))))))


;;;
;;; Symbol-macrolet statement
;;;

(defun env-add-symbol-macrolet-bindings (env bindings)
  (flet ((aux (env0 binding)
           (let* ((symbol (symbol-macrolet-binding-symbol binding))
                  (expansion (symbol-macrolet-binding-expansion binding)))
             (variable-environment-add-symbol-macro symbol expansion
                                                    env0))))
    (reduce #'aux bindings :initial-value env)))

(defun compile-symbol-macrolet-statements (statements env)
  (compile-statement `(progn ,@statements) env))

(defun compile-symbol-macrolet (form env)
  (let ((bindings (symbol-macrolet-bindings form))
        (statements (symbol-macrolet-statements form)))
    (let ((env1 (env-add-symbol-macrolet-bindings env bindings)))
      (let ((statements1 (compile-symbol-macrolet-statements statements env1)))
        (format nil "~a" statements1)))))


;;;
;;; Do statement
;;;

(defun env-add-do-bindings (env bindings)
  (flet ((aux (env0 binding)
           (let* ((var (do-binding-var binding))
                  (init (do-binding-init binding))
                  (type (type-of-expression init env)))
             (variable-environment-add-variable var type env0))))
    (reduce #'aux bindings :initial-value env)))

(defun compile-do-init-part (bindings env)
  (flet ((aux (binding)
           (let* ((var (do-binding-var binding))
                  (init (do-binding-init binding))
                  (type (type-of-expression init env)))
             (let ((var1 (compile-symbol var))
                   (init1 (compile-expression init env))
                   (type1 (compile-type type)))
               (format nil "~A ~A = ~A" type1 var1 init1)))))
    (format nil "~{~A~^, ~}" (mapcar #'aux bindings))))

(defun compile-do-test-part (end-test env)
  (let ((end-test1 (compile-expression end-test env)))
    (format nil "! ~A" end-test1)))

(defun compile-do-step-part (bindings env)
  (flet ((aux (binding)
           (let ((var (do-binding-var binding))
                 (step (do-binding-step binding)))
             (let ((var1 (compile-symbol var))
                   (step1 (compile-expression step env)))
               (format nil "~A = ~A" var1 step1)))))
    (format nil "~{~A~^, ~}" (mapcar #'aux bindings))))

(defun compile-do-statements (statements env)
  (compile-statement `(progn ,@statements) env))

(defun compile-do (form env)
  (let ((bindings (do-bindings form))
        (end-test (do-end-test form))
        (statements (do-statements form)))
    (let ((env1 (env-add-do-bindings env bindings)))
      (let ((init-part (compile-do-init-part bindings env))
            (test-part (compile-do-test-part end-test env1))
            (step-part (compile-do-step-part bindings env1))
            (statements1 (compile-do-statements statements env1)))
        (let ((statements2 (indent 2 statements1)))
          (format nil "for ( ~A; ~A; ~A )~%{~%~A}~%"
                      init-part test-part step-part statements2))))))


;;;
;;; With-local-memory statement
;;;

(defun env-add-with-local-memory-specs (env specs)
  (flet ((aux (env0 spec)
           (let* ((var (with-local-memory-spec-var spec))
                  (type (with-local-memory-spec-type spec))
                  (dims (length (with-local-memory-spec-dimensions spec))))
             (let ((type1 (array-type type dims)))
               (variable-environment-add-variable var type1 env0)))))
    (reduce #'aux specs :initial-value env)))

(defun compile-with-local-memory-spec-dimensions (dims env)
  (flet ((aux (dim)
           (compile-expression dim env)))
    (mapcar #'aux dims)))

(defun compile-with-local-memory-specs (specs env)
  (flet ((aux (spec)
           (let ((var (with-local-memory-spec-var spec))
                 (type (with-local-memory-spec-type spec))
                 (dims (with-local-memory-spec-dimensions spec)))
             (let ((var1 (compile-symbol var))
                   (type1 (compile-type type))
                   (dims1 (compile-with-local-memory-spec-dimensions
                            dims env)))
               ;; OpenCL v1.2 dr19: 6.5 Address Spece Qualifiers
               (format nil "__local ~A ~A~{[~A]~};~%" type1 var1 dims1)))))
    (format nil "~{~A~}" (mapcar #'aux specs))))

(defun compile-with-local-memory-statements (statements env)
  (compile-statement `(progn ,@statements) env))

(defun compile-with-local-memory (form env)
  (let ((specs (with-local-memory-specs form))
        (statements (with-local-memory-statements form)))
    (let ((env1 (env-add-with-local-memory-specs env specs)))
      (let ((specs1 (compile-with-local-memory-specs specs env))
            (statements1 (compile-with-local-memory-statements statements
                                                                env1)))
        (let ((specs2 (indent 2 specs1))
              (statements2 (indent 2 statements1)))
          (format nil "{~%~A~A}~%" specs2 statements2))))))


;;;
;;; Set statement
;;;

(defun compile-set (form env)
  (let ((reference (set-reference form))
        (expr (set-expression form)))
    ;; check if the reference part of set statement has the same type
    ;; as the expression part of that
    (let ((ref-type (type-of-expression reference env))
          (expr-type (type-of-expression expr env)))
      (unless (or (eq ref-type expr-type)
                  (and (scalar-float-type-p ref-type)
                       (scalar-integer-type-p expr-type)))
        (error "The type of statement ~S is type mismatch (~S and ~S)."
               form ref-type expr-type)))
    (let ((reference1 (compile-expression reference env))
          (expr1 (compile-expression expr env)))
      (format nil "~A = ~A;~%" reference1 expr1))))


;;;
;;; Progn statement
;;;

(defun compile-progn (form env)
  (flet ((aux (statement)
           (compile-statement statement env)))
    (let ((statements (progn-statements form)))
      (let ((statements1 (mapcar #'aux statements)))
        (format nil "~{~A~}" statements1)))))


;;;
;;; Return statement
;;;

(defun compile-return (form env)
  (let ((expr (return-expr form)))
    (if expr
        (let ((expr1 (compile-expression expr env)))
          (format nil "return ~A;~%" expr1))
        (format nil "return;~%"))))


;;;
;;; Function application
;;;

(defun compile-function (form env)
  (let ((code (oclcl.lang.compiler.compile-expression::compile-function
                form env)))
    (format nil "~A;~%" code)))


;;;
;;; Compiler directives
;;;

(defun compile-declare (form env)
  (declare (ignore env))
  (format nil "#~{~A~^ ~}~%" (cdr form)))