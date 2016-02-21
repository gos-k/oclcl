#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage :oclcl.lang
  (:use :cl
        :cl-reexport))
(in-package :oclcl.lang)

;; reexport symbols of data structures oclcl provides
(reexport-from :oclcl.lang.data
               :include '(;; Float3
                          :float3
                          :make-float3
                          :float3-x
                          :float3-y
                          :float3-z
                          :float3-p
                          :float3-=
                          ;; Float4
                          :float4
                          :make-float4
                          :float4-x
                          :float4-y
                          :float4-z
                          :float4-w
                          :float4-p
                          :float4-=
                          ;; Double3
                          :double3
                          :make-double3
                          :double3-x
                          :double3-y
                          :double3-z
                          :double3-p
                          :double3-=
                          ;; Double4
                          :double4
                          :make-double4
                          :double4-x
                          :double4-y
                          :double4-z
                          :double4-w
                          :double4-p
                          :double4-=))

;; reexport symbols of oclcl types
;; OpenCL v1.2 dr19: 6.1 Supported Data Type
(reexport-from :oclcl.lang.type
               :include '(:bool
                          :char :char2 :char3 :char4 :char8 :char16
                          :unsigned-char :uchar :uchar2 :uchar3 :uchar4 :uchar8 :uchar16
                          :short :short2 :short3 :short4 :short8 :short16
                          :unsigned-short :ushort :ushort2 :ushort3 :ushort4 :ushort8 :ushort16
                          :int :int2 :int3 :int4 :int8 :int16
                          :unsigned-int :uint :uint2 :uint3 :uint4 :uint8 :uint16
                          :long :long2 :long3 :long4 :long8 :long16
                          :unsigned-long :ulong :ulong2 :ulong3 :ulong4 :ulong8 :ulong16
                          :float :float2 :float3 :float4 :float8 :float16
                          :double :double2 :double3 :double4 :double8 :double16
                          :half
                          :size-t
                          :ptrdiff-t
                          :intptr-t
                          :uintptr-t
                          :void))

;; reexport symbols of oclcl syntax except the ones exported
;; from COMMON-LISP package
(reexport-from :oclcl.lang.syntax
               :include '(:clk-local-mem-fence
                          :clk-global-mem-fence
                          :with-local-memory
                          :set))

;; reexport symbols of oclcl built-in functions except the ones
;; exported from COMMON-LISP package
(reexport-from :oclcl.lang.built-in
               :include '(:get-work-dim
                          :get-global-size
                          :get-global-id
                          :get-local-size
                          :get-local-id
                          :get-num-groups
                          :get-group-id
                          :get-global-offset

                          :size-t-to-int

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

                          :half-cos
                          :half-divide
                          :half-exp
                          :half-exp2
                          :half-exp10
                          :half-log
                          :half-log2
                          :half-log10
                          :half-powr
                          :half-recip
                          :half-rsqrt
                          :half-sin
                          :half-sqrt
                          :half-tan

                          :native-cos
                          :native-divide
                          :native-exp
                          :native-exp2
                          :native-exp10
                          :native-log
                          :native-log2
                          :native-log10
                          :native-powr
                          :native-recip
                          :native-rsqrt
                          :native-sin
                          :native-sqrt
                          :native-tan

                          :abs
                          :abs-diff
                          :add-sat
                          :hadd
                          :rhadd
                          :clamp
                          :clz
                          :mad-hi
                          :mad-sat
                          :max
                          :min
                          :mul-hi
                          :rotate
                          :sub-sat
                          :upsampl
                          :popcount

                          :clamp
                          :degrees
                          :max
                          :min
                          :mix
                          :radians
                          :step
                          :smoothstep
                          :sign

                          :cross
                          :dot
                          :distance
                          :length
                          :normalize
                          :fast-distance
                          :fast-length
                          :fast-normalize

                          :barrier

                          :mem-fence
                          :read-mem-fence
                          :write-mem-fence))
