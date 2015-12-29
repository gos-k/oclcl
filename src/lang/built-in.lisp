#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.lang.built-in
  (:use :cl
        :oclcl.lang.type)
  (:export ;; Built-in functions
           :acos
           :acosh
           :acospi
           :asin
           :asinh
           :asinpi
           :atan
           :atan2
           :cbrt
           :ceil
           :copysign
           :cos
           :cosh
           :cospi
           :erfc
           :erf
           :exp
           :exp2
           :exp10
           :expm1
           :fabs
           :fdim
           :floor
           :fma
           :fmax
           :fmin
           :fmod
           :fract
           :frexp
           :hypot
           :ilogb
           :ldexp
           :lgamma
           :lgamma_r
           :log
           :log2
           :log10
           :log1p
           :logb
           :mad
           :maxmag
           :minmag
           :modf
           :nan
           :nextafter
           :pow
           :pown
           :powr
           :remainder
           :remquo
           :rint
           :rootn
           :round
           :rsqrt
           :sin
           :sincos
           :sinh
           :sinpi
           :sqrt
           :tan
           :tanh
           :tanpi
           :tgamma
           :trunc

           ;; Interfaces
           :built-in-function-return-type
           :built-in-function-infix-p
           :built-in-function-c-name))
(in-package :oclcl.lang.built-in)

(defparameter +signed-integer-types+ '(char short int long))
(defparameter +unsigned-integer-types+ '(uchar ushort uint ulong))
(defparameter +integer-types+ (append +signed-integer-types+ +unsigned-integer-types+))
(defparameter +float-types+ '(float double))
(defparameter +gentypes+ (append +integer-types+ +float-types+))

(defparameter +integer-result-types+ '(char char short short int int long long))
(defparameter +float-result-types+ '(int long))
(defparameter +result-gentypes+ (append +integer-result-types+ +float-result-types+))

(defun generate-vector-type-symbols (scalar-type)
  (loop for n in '("2" "3" "4" "8" "16")
        collecting (intern (concatenate 'string (symbol-name scalar-type) n))))

(defun same-type-function (function size type infix)
  (loop for type-symbol in (cons type (generate-vector-type-symbols type))
        collecting (list (make-list size :initial-element type-symbol) type-symbol infix function)))

(defun float-types-function (function size)
  (loop for type in +float-types+
        appending (same-type-function function size type nil)))

(defun float-types-unary-function (function)
  (float-types-function function 1))

(defun float-types-binary-function (function)
  (float-types-function function 2))

(defun float-types-ternary-function (function)
  (float-types-function function 3))

(defun same-types-binary-operator (operator types)
  (loop for type in types
        appending (same-type-function operator 2 type t)))

(defun scalar-vector-binary-operator (operator scalar-type)
  (loop for vector-type in (generate-vector-type-symbols scalar-type)
        collecting (list (list scalar-type vector-type) vector-type t operator)
        collecting (list (list vector-type scalar-type) vector-type t operator)))

(defun arithmetic-binary-operator (operator types)
  (loop for type in types
        appending (same-type-function operator 2 type t)
        appending (scalar-vector-binary-operator operator type)))

(defun vector-relational-operator (operator argument-type result-type)
  (loop for n in '("2" "3" "4" "8" "16")
        for argument-vector-type = (intern (concatenate 'string (symbol-name argument-type) n))
        for result-vector-type = (intern (concatenate 'string (symbol-name result-type) n))
        collecting (list (list argument-type argument-vector-type) result-vector-type t operator)
        collecting (list (list argument-vector-type argument-type) result-vector-type t operator)
        collecting (list (list argument-vector-type argument-vector-type) result-vector-type t operator)))

(defun relational-operator (operator)
  (loop for argument-type in +gentypes+
        for result-type in +result-gentypes+
        ;; spec is 'int but ...
        collecting (list (list argument-type argument-type) 'bool t operator)
        appending (vector-relational-operator operator argument-type result-type)))

;;;
;;; Built-in functions
;;;

(defparameter +built-in-functions+
  `(;; arithmetic operators
    + ,(arithmetic-binary-operator "+" +gentypes+)
    - ,(append '(((int) int nil "-"))
               (arithmetic-binary-operator "-" +gentypes+))
    * ,(arithmetic-binary-operator "*" +gentypes+)
    / ,(arithmetic-binary-operator "/" +gentypes+)
    mod ,(arithmetic-binary-operator "%" +integer-types+)

    ;; relational operators
    = ,(relational-operator "==")
    /= ,(relational-operator "!=")
    < ,(relational-operator "<")
    > ,(relational-operator ">")
    <= ,(relational-operator "<=")
    >= ,(relational-operator ">=")

    ;; logical operators
    not  (((bool) bool nil "!"))
    ;; atomic functions
    atomic-add (((int* int) int nil "atomicAdd"))
    ;; address-of operator
    pointer (((int)   int*   nil "&")
             ((float) float* nil "&")
             ((double) double* nil "&")
             ((curand-state-xorwow) curand-state-xorwow* nil "&"))
    ;; built-in vector constructor
    float3 (((float float float) float3 nil "make_float3"))
    float4 (((float float float float) float4 nil "make_float4"))
    double3 (((double double double) double3 nil "make_double3"))
    double4 (((double double double double) double4 nil "make_double4"))
    ;; Synchronization functions
    syncthreads ((() void nil "__syncthreads"))
    ;; type casting intrinsics
    double-to-int-rn (((double) int nil "__double2int_rn"))
    ;; linear algebraic operators
    dot (((float3 float3) float nil "float3_dot")
         ((float4 float4) float nil "float4_dot")
         ((double3 double3) double nil "double3_dot")
         ((double4 double4) double nil "double4_dot"))

    ;; OpenCL v1.2 dr19: 6.12.1 Work-Item Functions
    get-work-dim ((() uint nil "get_work_dim"))
    get-global-size (((uint) size-t nil "get_global_size"))
    get-global-id (((uint) size-t nil "get_global_id"))
    get-local-size (((int) size-t nil "get_local_size"))
    get-local-id (((int) size-t nil "get_local_id"))
    get-num-groups (((uint) size-t nil "get_num_groups"))
    get-group-id (((uint) size-t nil "get_group_id"))
    get-global-offset (((uint) size-t nil "get_global_offset"))

    ;; OpenCL v.1.2 dr19: 6.12.2 Math Functions
    acos ,(float-types-unary-function "acos")
    acosh ,(float-types-unary-function "acosh")
    acospi ,(float-types-unary-function "acospi")
    asin ,(float-types-unary-function "asin")
    asinh ,(float-types-unary-function "asinh")
    asinpi ,(float-types-unary-function "asinpi")
    atan ,(float-types-unary-function "atan")
    atan2 ,(float-types-binary-function "atan2")
    cbrt ,(float-types-unary-function "cbrt")
    ceil ,(float-types-unary-function "ceil")
    copysign ,(float-types-binary-function "copysign")
    cos ,(float-types-unary-function "cos")
    cosh ,(float-types-unary-function "cosh")
    cospi ,(float-types-unary-function "cospi")
    erfc ,(float-types-unary-function "erfc")
    erf ,(float-types-unary-function "erf")
    exp ,(float-types-unary-function "exp")
    exp2 ,(float-types-unary-function "exp2")
    exp10 ,(float-types-unary-function "exp10")
    expm1 ,(float-types-unary-function "expm1")
    fabs ,(float-types-unary-function "fabs")
    fdim ,(float-types-binary-function "fdim")
    floor ,(float-types-unary-function "floor")
    fma ,(float-types-ternary-function "fma")
    ;;fmax ,(float-types-binary-function "fmax")
    ;;fmin ,(float-types-binary-function "fmin")
    fmod ,(float-types-binary-function "fmod")
    ;;fract
    ;;frexp
    hypot ,(float-types-binary-function "hypot")
    ;;ilogb
    ;;ldexp
    lgamma ,(float-types-unary-function "lgamma")
    ;;lgamma_r
    log ,(float-types-unary-function "log")
    log2 ,(float-types-unary-function "log2")
    log10 ,(float-types-unary-function "log10")
    log1p ,(float-types-unary-function "log1p")
    logb ,(float-types-unary-function "logb")
    mad ,(float-types-ternary-function "mad")
    maxmag ,(float-types-unary-function "maxmag")
    minmag ,(float-types-unary-function "minmag")
    ;;modf
    ;;nan
    nextafter ,(float-types-binary-function "nextafter")
    pow ,(float-types-binary-function "pow")
    ;;pown
    powr ,(float-types-binary-function "powr")
    remainder ,(float-types-binary-function "remainder")
    ;;remquo
    rint ,(float-types-unary-function "rint")
    ;;rootn
    round ,(float-types-unary-function "round")
    rsqrt ,(float-types-unary-function "rsqrt")
    sin ,(float-types-unary-function "sin")
    ;;sincos
    sinh ,(float-types-unary-function "sinh")
    sinpi ,(float-types-unary-function "sinpi")
    sqrt ,(float-types-unary-function "sqrt")
    tan ,(float-types-unary-function "tan")
    tanh ,(float-types-unary-function "tanh")
    tanpi ,(float-types-unary-function "tanpi")
    tgamma ,(float-types-unary-function "tgamma")
    trunc ,(float-types-unary-function "trunc")))

(defun inferred-function-candidates (name)
  (or (getf +built-in-functions+ name)
      (error "The function ~S is undefined." name)))

(defun inferred-function (name argument-types)
  (let ((candidates (inferred-function-candidates name)))
    (or (assoc argument-types candidates :test #'equal)
        (error "The function ~S is undefined." name))))

(defun built-in-function-return-type (name argument-types)
  (cadr (inferred-function name argument-types)))

(defun built-in-function-infix-p (name argument-types)
  (caddr (inferred-function name argument-types)))

(defun built-in-function-c-name (name argument-types)
  (cadddr (inferred-function name argument-types)))
