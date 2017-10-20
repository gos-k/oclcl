#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
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

(defun compile-statement (form var-env func-env)
  (cond
    ((%macro-p form func-env) (compile-macro form var-env func-env))
    ((if-p form) (compile-if form var-env func-env))
    ((let-p form) (compile-let form var-env func-env))
    ((symbol-macrolet-p form) (compile-symbol-macrolet form var-env func-env))
    ((do-p form) (compile-do form var-env func-env))
    ((with-local-memory-p form)
     (compile-with-local-memory form var-env func-env))
    ((set-p form) (compile-set form var-env func-env))
    ((progn-p form) (compile-progn form var-env func-env))
    ((return-p form) (compile-return form var-env func-env))
    ((declare-p form) (compile-declare form var-env func-env))
    ((function-p form) (compile-function form var-env func-env))
    (t (error "The value ~S is an invalid statement." form))))


;;;
;;; Macro
;;;

(defun %macro-p (form func-env)
  (oclcl.lang.compiler.compile-expression::%macro-p form func-env))

(defun compile-macro (form var-env func-env)
  (let ((operator (macro-operator form))
        (operands (macro-operands form)))
    (let ((expander (function-environment-macro-expander func-env operator)))
      (let ((form1 (funcall expander operands)))
        (compile-statement form1 var-env func-env)))))


;;;
;;; If statement
;;;

(defun compile-if (form var-env func-env)
  (let ((test-expr (if-test-expression form))
        (then-stmt (if-then-statement form))
        (else-stmt (if-else-statement form)))
    ;; check if the test part of inline-if expression has bool type
    (let ((test-type (type-of-expression test-expr var-env func-env)))
      (unless (eq test-type 'bool)
        (error "The type of statement ~S is invalid." form)))
    (let ((test-expr1 (compile-expression test-expr var-env func-env))
          (then-stmt1 (compile-statement then-stmt var-env func-env))
          (else-stmt1 (if else-stmt
                          (compile-statement else-stmt var-env func-env))))
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

(defun var-env-add-let-bindings (var-env func-env bindings)
  (flet ((aux (var-env0 binding)
           (let* ((var (let-binding-var binding))
                  (expr (let-binding-expr binding))
                  (type (type-of-expression expr var-env func-env)))
             (variable-environment-add-variable var type var-env0))))
    (reduce #'aux bindings :initial-value var-env)))

(defun compile-let-bindings (bindings var-env func-env)
  (flet ((aux (binding)
           (let* ((var (let-binding-var binding))
                  (expr (let-binding-expr binding))
                  (type (type-of-expression expr var-env func-env)))
             (let ((var1 (compile-symbol var))
                   (expr1 (compile-expression expr var-env func-env))
                   (type1 (compile-type type)))
               (format nil "~A ~A = ~A;~%" type1 var1 expr1)))))
    (format nil "~{~A~}" (mapcar #'aux bindings))))

(defun compile-let-statements (statements var-env func-env)
  (compile-statement `(progn ,@statements) var-env func-env))

(defun compile-let (form var-env func-env)
  (let ((bindings (let-bindings form))
        (statements (let-statements form)))
    (let ((var-env1 (var-env-add-let-bindings var-env func-env bindings)))
      (let ((bindings1 (compile-let-bindings bindings var-env func-env))
            (statements1 (compile-let-statements statements var-env1
                                                            func-env)))
        (let ((bindings2 (indent 2 bindings1))
              (statements2 (indent 2 statements1)))
          (format nil "{~%~A~A}~%" bindings2 statements2))))))


;;;
;;; Symbol-macrolet statement
;;;

(defun var-env-add-symbol-macrolet-bindings (var-env bindings)
  (flet ((aux (var-env0 binding)
           (let* ((symbol (symbol-macrolet-binding-symbol binding))
                  (expansion (symbol-macrolet-binding-expansion binding)))
             (variable-environment-add-symbol-macro symbol expansion
                                                    var-env0))))
    (reduce #'aux bindings :initial-value var-env)))

(defun compile-symbol-macrolet-statements (statements var-env func-env)
  (compile-statement `(progn ,@statements) var-env func-env))

(defun compile-symbol-macrolet (form var-env func-env)
  (let ((bindings (symbol-macrolet-bindings form))
        (statements (symbol-macrolet-statements form)))
    (let ((var-env1 (var-env-add-symbol-macrolet-bindings var-env
                                                          bindings)))
      (let ((statements1 (compile-symbol-macrolet-statements statements
                                                             var-env1
                                                             func-env)))
        (format nil "~a" statements1)))))


;;;
;;; Do statement
;;;

(defun var-env-add-do-bindings (var-env func-env bindings)
  (flet ((aux (var-env0 binding)
           (let* ((var (do-binding-var binding))
                  (init (do-binding-init binding))
                  (type (type-of-expression init var-env func-env)))
             (variable-environment-add-variable var type var-env0))))
    (reduce #'aux bindings :initial-value var-env)))

(defun compile-do-init-part (bindings var-env func-env)
  (flet ((aux (binding)
           (let* ((var (do-binding-var binding))
                  (init (do-binding-init binding))
                  (type (type-of-expression init var-env func-env)))
             (let ((var1 (compile-symbol var))
                   (init1 (compile-expression init var-env func-env))
                   (type1 (compile-type type)))
               (format nil "~A ~A = ~A" type1 var1 init1)))))
    (format nil "~{~A~^, ~}" (mapcar #'aux bindings))))

(defun compile-do-test-part (end-test var-env func-env)
  (let ((end-test1 (compile-expression end-test var-env func-env)))
    (format nil "! ~A" end-test1)))

(defun compile-do-step-part (bindings var-env func-env)
  (flet ((aux (binding)
           (let ((var (do-binding-var binding))
                 (step (do-binding-step binding)))
             (let ((var1 (compile-symbol var))
                   (step1 (compile-expression step var-env func-env)))
               (format nil "~A = ~A" var1 step1)))))
    (format nil "~{~A~^, ~}" (mapcar #'aux bindings))))

(defun compile-do-statements (statements var-env func-env)
  (compile-statement `(progn ,@statements) var-env func-env))

(defun compile-do (form var-env func-env)
  (let ((bindings (do-bindings form))
        (end-test (do-end-test form))
        (statements (do-statements form)))
    (let ((var-env1 (var-env-add-do-bindings var-env func-env bindings)))
      (let ((init-part (compile-do-init-part bindings var-env func-env))
            (test-part (compile-do-test-part end-test var-env1 func-env))
            (step-part (compile-do-step-part bindings var-env1 func-env))
            (statements1 (compile-do-statements statements var-env1
                                                           func-env)))
        (let ((statements2 (indent 2 statements1)))
          (format nil "for ( ~A; ~A; ~A )~%{~%~A}~%"
                      init-part test-part step-part statements2))))))


;;;
;;; With-local-memory statement
;;;

(defun var-env-add-with-local-memory-specs (var-env specs)
  (flet ((aux (var-env0 spec)
           (let* ((var (with-local-memory-spec-var spec))
                  (type (with-local-memory-spec-type spec))
                  (dims (length (with-local-memory-spec-dimensions spec))))
             (let ((type1 (array-type type dims)))
               (variable-environment-add-variable var type1 var-env0)))))
    (reduce #'aux specs :initial-value var-env)))

(defun compile-with-local-memory-spec-dimensions (dims var-env func-env)
  (flet ((aux (dim)
           (compile-expression dim var-env func-env)))
    (mapcar #'aux dims)))

(defun compile-with-local-memory-specs (specs var-env func-env)
  (flet ((aux (spec)
           (let ((var (with-local-memory-spec-var spec))
                 (type (with-local-memory-spec-type spec))
                 (dims (with-local-memory-spec-dimensions spec)))
             (let ((var1 (compile-symbol var))
                   (type1 (compile-type type))
                   (dims1 (compile-with-local-memory-spec-dimensions
                            dims var-env func-env)))
               ;; OpenCL v1.2 dr19: 6.5 Address Spece Qualifiers
               (format nil "__local ~A ~A~{[~A]~};~%" type1 var1 dims1)))))
    (format nil "~{~A~}" (mapcar #'aux specs))))

(defun compile-with-local-memory-statements (statements var-env func-env)
  (compile-statement `(progn ,@statements) var-env func-env))

(defun compile-with-local-memory (form var-env func-env)
  (let ((specs (with-local-memory-specs form))
        (statements (with-local-memory-statements form)))
    (let ((var-env1 (var-env-add-with-local-memory-specs var-env specs)))
      (let ((specs1 (compile-with-local-memory-specs specs var-env func-env))
            (statements1 (compile-with-local-memory-statements statements
                                                                var-env1
                                                                func-env)))
        (let ((specs2 (indent 2 specs1))
              (statements2 (indent 2 statements1)))
          (format nil "{~%~A~A}~%" specs2 statements2))))))


;;;
;;; Set statement
;;;

(defun compile-set (form var-env func-env)
  (let ((reference (set-reference form))
        (expr (set-expression form)))
    ;; check if the reference part of set statement has the same type
    ;; as the expression part of that
    (let ((ref-type (type-of-expression reference var-env func-env))
          (expr-type (type-of-expression expr var-env func-env)))
      (unless (eq ref-type expr-type)
        (error "The type of statement ~S is type mismatch (~S and ~S)."
               form ref-type expr-type)))
    (let ((reference1 (compile-expression reference var-env func-env))
          (expr1 (compile-expression expr var-env func-env)))
      (format nil "~A = ~A;~%" reference1 expr1))))


;;;
;;; Progn statement
;;;

(defun compile-progn (form var-env func-env)
  (flet ((aux (statement)
           (compile-statement statement var-env func-env)))
    (let ((statements (progn-statements form)))
      (let ((statements1 (mapcar #'aux statements)))
        (format nil "~{~A~}" statements1)))))


;;;
;;; Return statement
;;;

(defun compile-return (form var-env func-env)
  (let ((expr (return-expr form)))
    (if expr
        (let ((expr1 (compile-expression expr var-env func-env)))
          (format nil "return ~A;~%" expr1))
        (format nil "return;~%"))))


;;;
;;; Function application
;;;

(defun compile-function (form var-env func-env)
  (let ((code (oclcl.lang.compiler.compile-expression::compile-function
                form var-env func-env)))
    (format nil "~A;~%" code)))


;;;
;;; Compiler directives
;;;

(defun compile-declare (form var-env func-env)
  (declare (ignore var-env func-env))
  (format nil "#~{~A~^ ~}~%" (cdr form)))


