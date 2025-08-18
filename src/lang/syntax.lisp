#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.syntax
  (:use :cl
        :oclcl.lang.data
        :oclcl.lang.type)
  (:export ;; Symbol macro
           :symbol-macro-p
           ;; Macro
           :macro-p
           :macro-operator
           :macro-operands
           ;; Literal
           :literal-p
           :bool-literal-p
           :int-literal-p
           :float-literal-p
           :double-literal-p
           :string-literal-p
           ;; OpenCL literal
           :opencl-literal-p
           :cl-mem-fence-flags-literal-p
           :clk-local-mem-fence
           :clk-global-mem-fence
           ;; Reference
           :reference-p
           ;; Reference - Variable
           :variable-reference-p
           ;; Reference - Structure
           :structure-reference-p
           :structure-reference-accessor
           :structure-reference-expr
           ;; Reference - Array
           :array-reference-p
           :array-reference-expr
           :array-reference-indices
           ;; Inline-if
           :inline-if-p
           :inline-if-test-expression
           :inline-if-then-expression
           :inline-if-else-expression
           ;; Arithmetic
           :arithmetic-p
           :arithmetic-operator
           :arithmetic-operands
           ;; Function application
           :function-p
           :function-operator
           :function-operands
           ;; If statement
           :if-p
           :if-test-expression
           :if-then-statement
           :if-else-statement
           ;; Let statement
           :let-p
           :let-bindings
           :let-statements
           ;; Let statement - binding
           :let-binding-p
           :let-binding-var
           :let-binding-expr
           ;; Symbol-macrolet statement
           :symbol-macrolet-p
           :symbol-macrolet-bindings
           :symbol-macrolet-statements
           ;; Symbol-macrolet statement - binding
           :symbol-macrolet-binding-p
           :symbol-macrolet-binding-symbol
           :symbol-macrolet-binding-expansion
           ;; Do statement
           :do-p
           :do-bindings
           :do-end-test
           :do-statements
           ;; Do statement - binding
           :do-binding-p
           :do-binding-var
           :do-binding-init
           :do-binding-step
           ;; With-local-memory statement
           :with-local-memory
           :with-local-memory-p
           :with-local-memory-specs
           :with-local-memory-statements
           ;; With-local-memory statement - spec
           :with-local-memory-spec-p
           :with-local-memory-spec-var
           :with-local-memory-spec-type
           :with-local-memory-spec-dimensions
           ;; Set statement
           :set
           :set-p
           :set-reference
           :set-expression
           ;; Progn statement
           :progn-p
           :progn-statements
           ;; Return statement
           :return-p
           :return-expr
           ;; Argument
           :argument
           :argument-p
           :argument-var
           :argument-type
           ;; Compiler directive
           :declare-p
           ;; sizeof
           :sizeof
           :sizeof-p
           :sizeof-operand))
(in-package :oclcl.lang.syntax)


;;;
;;; Symbol macro
;;;

(defun symbol-macro-p (form)
  (oclcl-symbol-p form))


;;;
;;; Macro
;;;

(defun macro-p (form)
  (cl-pattern:match form
    ((name . _) (oclcl-symbol-p name))
    (_ nil)))

(defun macro-operator (form)
  (unless (macro-p form)
    (error "The value ~S is an invalid form." form))
  (car form))

(defun macro-operands (form)
  (unless (macro-p form)
    (error "The value ~S is an invalid form." form))
  (cdr form))


;;;
;;; Literal
;;;

(defun literal-p (form)
  (or (bool-literal-p form)
      (int-literal-p form)
      (float-literal-p form)
      (double-literal-p form)
      (string-literal-p form)))

(defun bool-literal-p (form)
  (oclcl-bool-p form))

(defun int-literal-p (form)
  (oclcl-int-p form))

(defun float-literal-p (form)
  (oclcl-float-p form))

(defun double-literal-p (form)
  (oclcl-double-p form))

(defun string-literal-p (form)
  (oclcl-string-p form))

;;; OpenCL literal
;;;

(defun opencl-literal-p (form)
  (or (cl-mem-fence-flags-literal-p form)))

(defun cl-mem-fence-flags-literal-p (form)
  (find form '(:clk-local-mem-fence
               :clk-global-mem-fence)))

;;;
;;; Reference
;;;

(defun reference-p (form)
  (or (variable-reference-p form)
      (structure-reference-p form)
      (array-reference-p form)))


;;;
;;; Reference - Variable
;;;

(defun variable-reference-p (form)
  (oclcl-symbol-p form))


;;;
;;; Reference - Structure
;;;

(defun structure-reference-p (form)
  (cl-pattern:match form
    ((accessor _) (structure-accessor-p accessor))
    (_ nil)))

(defun structure-reference-accessor (form)
  (unless (structure-reference-p form)
    (error "The form ~S is invalid." form))
  (car form))

(defun structure-reference-expr (form)
  (unless (structure-reference-p form)
    (error "The form ~S is invalid." form))
  (cadr form))


;;;
;;; Reference - Array
;;;

(defun array-reference-p (form)
  (cl-pattern:match form
    (('aref . _) t)
    (_ nil)))

(defun array-reference-expr (form)
  (cl-pattern:match form
    (('aref expr _ . _) expr)
    (('aref . _) (error "The expression ~S is malformed." form))
    (_ (error "The value ~S is an invalid expression." form))))

(defun array-reference-indices (form)
  (cl-pattern:match form
    (('aref _ . indices) (or indices
                             (error "The expression ~S is malformed." form)))
    (('aref) (error "The expression ~S is malformed." form))
    (_ (error "The value ~S is an invalid expression." form))))


;;;
;;; Inline-if
;;;

(defun inline-if-p (form)
  (cl-pattern:match form
    (('if . _) t)
    (_ nil)))

(defun inline-if-test-expression (form)
  (cl-pattern:match form
    (('if test-expr _ _) test-expr)
    (('if . _) (error "The expression ~S is malformed." form))
    (_ (error "The value ~S is an invalid expression." form))))

(defun inline-if-then-expression (form)
  (cl-pattern:match form
    (('if _ then-expr _) then-expr)
    (('if . _) (error "The expression ~S is malformed." form))
    (_ (error "The value ~S is an invalid expression." form))))

(defun inline-if-else-expression (form)
  (cl-pattern:match form
    (('if _ _ else-expr) else-expr)
    (('if . _) (error "The expression ~S is malformed." form))
    (_ (error "The value ~S is an invalid expression." form))))


;;;
;;; Arithmetic
;;;

(defparameter +arithmetic-operators+
  '(+ - * / mod))

(defun arithmetic-p (form)
  (cl-pattern:match form
    ((name . _) (and (member name +arithmetic-operators+)
                     t))
    (_ nil)))

(defun arithmetic-operator (form)
  (unless (arithmetic-p form)
    (error "The form ~S is invalid." form))
  (car form))

(defun arithmetic-operands (form)
  (unless (arithmetic-p form)
    (error "The form ~S is invalid." form))
  (cdr form))


;;;
;;; Function appication
;;;

(defun function-p (form)
  (cl-pattern:match form
    ((name . _) (oclcl-symbol-p name))
    (_ nil)))

(defun function-operator (form)
  (unless (function-p form)
    (error "The form ~S is invalid." form))
  (car form))

(defun function-operands (form)
  (unless (function-p form)
    (error "The form ~S is invalid." form))
  (cdr form))


;;;
;;; If statement
;;;

(defun if-p (form)
  (inline-if-p form))

(defun if-test-expression (form)
  (cl-pattern:match form
    (('if _ _ _ _ . _) (error "The statement ~S is malformed." form))
    (('if test-expr _ . _) test-expr)
    (('if . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))

(defun if-then-statement (form)
  (cl-pattern:match form
    (('if _ _ _ _ . _) (error "The statement ~S is malformed." form))
    (('if _ then-stmt . _) then-stmt)
    (('if . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))

(defun if-else-statement (form)
  (cl-pattern:match form
    (('if _ _ _ _ . _) (error "The statement ~S is malformed." form))
    (('if _ _ else-stmt) else-stmt)
    (('if _ _) nil)
    (('if . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))


;;;
;;; Let statement
;;;

(defun let-p (form)
  (cl-pattern:match form
    (('let . _) t)
    (_ nil)))

(defun let-bindings (form)
  (cl-pattern:match form
    (('let bindings . _)
     (if (every #'let-binding-p bindings)
         bindings
         (error "The statement ~S is malformed." form)))
    (('let . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))

(defun let-statements (form)
  (cl-pattern:match form
    (('let _ . statements) statements)
    (('let . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))


;;;
;;; Let statement - binding
;;;

(defun let-binding-p (object)
  (cl-pattern:match object
    ((var _) (oclcl-symbol-p var))
    (_ nil)))

(defun let-binding-var (binding)
  (unless (let-binding-p binding)
    (error "The value ~S is an invalid binding." binding))
  (car binding))

(defun let-binding-expr (binding)
  (unless (let-binding-p binding)
    (error "The value ~S is an invalid binding." binding))
  (cadr binding))


;;;
;;; Symbol-macrolet statement
;;;

(defun symbol-macrolet-p (form)
  (cl-pattern:match form
    (('symbol-macrolet . _) t)
    (_ nil)))

(defun symbol-macrolet-bindings (form)
  (cl-pattern:match form
    (('symbol-macrolet bindings . _)
     (if (every #'symbol-macrolet-binding-p bindings)
         bindings
         (error "The statement ~S is malformed." form)))
    (('symbol-macrolet . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))

(defun symbol-macrolet-statements (form)
  (cl-pattern:match form
    (('symbol-macrolet _ . statements) statements)
    (('symbol-macrolet . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))


;;;
;;; Symbol-macrolet statement - binding
;;;

(defun symbol-macrolet-binding-p (object)
  (let-binding-p object))

(defun symbol-macrolet-binding-symbol (binding)
  (let-binding-var binding))

(defun symbol-macrolet-binding-expansion (binding)
  (let-binding-expr binding))


;;;
;;; Do statement
;;;

(defun do-p (form)
  (cl-pattern:match form
    (('do . _) t)
    (_ nil)))

(defun do-bindings (form)
  (cl-pattern:match form
    (('do bindings _ . _)
     (if (every #'do-binding-p bindings)
         bindings
         (error "The statement ~S is malformed." form)))
    (('do . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))

(defun do-end-test (form)
  (cl-pattern:match form
    (('do _ (end-test) . _) end-test)
    (('do . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))

(defun do-statements (form)
  (cl-pattern:match form
    (('do _ _ . statements) statements)
    (('do . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))


;;;
;;; Do statement - binding
;;;

(defun do-binding-p (object)
  (cl-pattern:match object
    ((var _) (oclcl-symbol-p var))
    ((var _ _) (oclcl-symbol-p var))
    (_ nil)))

(defun do-binding-var (binding)
  (unless (do-binding-p binding)
    (error "The value ~S is an invalid binding." binding))
  (car binding))

(defun do-binding-init (binding)
  (unless (do-binding-p binding)
    (error "The value ~S is an invalid binding." binding))
  (cadr binding))

(defun do-binding-step (binding)
  (unless (do-binding-p binding)
    (error "The value ~S is an invalid binding." binding))
  (caddr binding))


;;;
;;; With-local-memory statement
;;;

(defun with-local-memory-p (object)
  (cl-pattern:match object
    (('with-local-memory . _) t)
    (_ nil)))

(defun with-local-memory-specs (form)
  (cl-pattern:match form
    (('with-local-memory specs . _)
     (if (every #'with-local-memory-spec-p specs)
         specs
         (error "The statement ~S is malformed." form)))
    (('with-local-memory . _)
     (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))

(defun with-local-memory-statements (form)
  (cl-pattern:match form
    (('with-local-memory _ . statements) statements)
    (('with-local-memory . _)
     (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))


;;;
;;; With-local-memory statement - spec
;;;

(defun with-local-memory-spec-p (object)
  (cl-pattern:match object
    ((var type . _) (and (oclcl-symbol-p var)
                         (oclcl-type-p type)))
    (_ nil)))

(defun with-local-memory-spec-var (spec)
  (unless (with-local-memory-spec-p spec)
    (error "The value ~S is an invalid shared memory spec." spec))
  (car spec))

(defun with-local-memory-spec-type (spec)
  (unless (with-local-memory-spec-p spec)
    (error "The value ~S is an invalid shared memory spec." spec))
  (cadr spec))

(defun with-local-memory-spec-dimensions (spec)
  (unless (with-local-memory-spec-p spec)
    (error "The value ~S is an invalid shared memory spec." spec))
  (cddr spec))


;;;
;;; Set statement
;;;

(defun set-p (object)
  (cl-pattern:match object
    (('set _ _) t)
    (_ nil)))

(defun set-reference (form)
  (cl-pattern:match form
    (('set reference _) (if (reference-p reference)
                            reference
                            (error "The statement ~S is malformed." form)))
    (('set . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))

(defun set-expression (form)
  (cl-pattern:match form
    (('set _ expr) expr)
    (('set . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))

;;;
;;; Progn statement
;;;

(defun progn-p (object)
  (cl-pattern:match object
    (('progn . _) t)
    (_ nil)))

(defun progn-statements (form)
  (cl-pattern:match form
    (('progn . statements) statements)
    (_ (error "The value ~S is an invalid statement." form))))


;;;
;;; Return statement
;;;

(defun return-p (object)
  (cl-pattern:match object
    (('return) t)
    (('return _) t)
    (_ nil)))

(defun return-expr (form)
  (cl-pattern:match form
    (('return) nil)
    (('return expr) expr)
    (('return . _) (error "The statement ~S is malformed." form))
    (_ (error "The value ~S is an invalid statement." form))))


;;;
;;; Argument
;;;

(deftype argument ()
  `(satisfies argument-p))

(defun argument-p (object)
  (cl-pattern:match object
    ((var type) (and (oclcl-symbol-p var)
                     (oclcl-type-p type)))
    (_ nil)))

(defun argument-var (argument)
  (unless (argument-p argument)
    (error "The value ~A is an invalid argument." argument))
  (car argument))

(defun argument-type (argument)
  (unless (argument-p argument)
    (error "The value ~A is an invalid argument." argument))
  (cadr argument))

;;;
;;; Compiler directive
;;;

(defun declare-p (form)
  (cl-pattern:match form
    (('declare . _) t)
    (_ nil)))

;;;
;;; Compiler sizeof
;;;

(defun sizeof-p (form)
  (cl-pattern:match form
    (('sizeof . _) t)
    (_ nil)))

(defun sizeof-operand (form)
  (unless (sizeof-p form)
    (error "The form ~S is invalid." form))
  (cdr form))
