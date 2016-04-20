# oclcl

[![CircleCI Status](https://circleci.com/gh/gos-k/oclcl.svg?style=shield)](https://circleci.com/gh/gos-k/oclcl)
[![TravisCI Status](https://travis-ci.org/gos-k/oclcl.svg?branch=master)](https://travis-ci.org/gos-k/oclcl)
[![Quicklisp](http://quickdocs.org/badge/oclcl.svg)](http://quickdocs.org/oclcl/)

oclcl is a library to use OpenCL in Common Lisp programs.
It provides the kernel description language with which users can define OpenCL kernel functions in S-expression.
The kernel description language also provides facilities to define kernel macros and kernel symbol macros in addition to kernel functions.
oclcl's kernel macro and kernel symbol macro offer powerful abstraction that OpenCL C itself does not have and provide enormous advantage in resource-limited GPU programming.

## Installation

If you use roswell,

    $ cd ~/.roswell/local-projects
    $ git clone git://github.com/gos-k/oclcl.git

Then `(ql:quickload :oclcl)` from `REPL` to load it.

## Requirements

oclcl requires following:

* OpenCL 1.2
* SBCL 1.3.1

## Test

    (ql:quickload :prove)
    (prove:run :oclcl-test)

or

    $ ros install prove
    $ run-prove oclcl/oclcl-test.asd

## Verification environments

oclcl is verified to work in following environments:

### Environment 1

* Ubuntu 15.04 x86_64
* Intel Core i5-4210U
* POCL 0.10
* SBCL 1.3.1 64-bit
* Roswell 0.0.3.50

### Environment 2

* Ubuntu 16.04 x86\_64
* NVIDIA GeForce GTX 660
* OpenCL 1.2 CUDA 8.0.20
* SBCL 1.3.4 64-bit
* Roswell 0.0.5.59

### Environment 3

* Ubuntu 14.04 x86\_64
* AMD Radeon HD 5700 Series
* OpenCL C 1.2
* SBCL 1.3.2 64-bit
* Roswell 0.0.5.58

## Kernel Description Language

### Types

Support types.

* `char` `char2` `char3` `char4` `char8` `char16`
* `char*` `char2*` `char3*` `char4*` `char8*` `char16*`
* `uchar` `uchar2` `uchar3` `uchar4` `uchar8` `uchar16`
* `uchar*` `uchar2*` `uchar3*` `uchar4*` `uchar8*` `uchar16*`
* `short` `short2` `short3` `short4` `short8` `short16`
* `short*` `short2*` `short3*` `short4*` `short8*` `short16*`
* `ushort` `ushort2` `ushort3` `ushort4` `ushort8` `ushort16`
* `ushort*` `ushort2*` `ushort3*` `ushort4*` `ushort8*` `ushort16*`
* `int` `int2` `int3` `int4` `int8` `int16`
* `int*` `int2*` `int3*` `int4*` `int8*` `int16*`
* `uint` `uint2` `uint3` `uint4` `uint8` `uint16`
* `uint*` `uint2*` `uint3*` `uint4*` `uint8*` `uint16*`
* `long` `long2` `long3` `long4` `long8` `long16`
* `long*` `long2*` `long3*` `long4*` `long8*` `long16*`
* `ulong` `ulong2` `ulong3` `ulong4` `ulong8` `ulong16`
* `ulong*` `ulong2*` `ulong3*` `ulong4*` `ulong8*` `ulong16*`
* `float` `float2` `float3` `float4` `float8` `float16`
* `float*` `float2*` `float3*` `float4*` `float8*` `float16*`
* `double` `double2` `double3` `double4` `double8` `double16`
* `double*` `double2*` `double3*` `double4*` `double8*` `double16*`
* `bool` `void` `size-t`

### IF statement

    IF test-form then-form [else-form]

`if` allows the execution of a form to be dependent on a single `test-form`. First `test-form` is evaluated. If the result is `true`, then `then-form` is selected; otherwise `else-form` is selected. Whichever form is selected is then evaluated. If `else-form` is not provided, does nothing when `else-form` is selected.

Example:

    (if (= a 0)
        (return 0)
        (return 1))

Compiled:

    if (a == 0) {
      return 0;
    } else {
      return 1;
    }

### LET statement

    LET ({(var init-form)}*) statement*

`let` declares new variable bindings and set corresponding `init-form`s to them and execute a series of `statement`s that use these bindings. `let` performs the bindings in parallel. For sequentially, use `let*` kernel macro instead.

Example:

    (let ((i 0))
      (return i))

Compiled:

    {
      int i = 0;
      return i;
    }

### SYMBOL-MACROLET statement

    SYMBOL-MACROLET ({(symbol expansion)}*) statement*

`symbol-macrolet` establishes symbol expansion rules in the variable environment and execute a series of `statement`s that use these rules. In cl-cuda's compilation process, the symbol macros found in a form are replaces by corresponding `expansion`s.

Example:

    (symbol-macrolet ((x 1.0))
      (return x))

Compiled:

    {
      return 1.0;
    }

### DO statement

    DO ({(var init-form step-form)}*) (test-form) statement*

`do` iterates over a group of `statement`s while `test-form` holds. `do` accepts an arbitrary number of iteration `var`s and their initial values are supplied by `init-form`s. `step-form`s supply how the `var`s should be updated on succeeding iterations through the loop.

Example:

    (do ((a 0 (+ a 1))
         (b 0 (+ b 1)))
        ((> a 15))
      (do-some-statement))

Compiled:

    for ( int a = 0, int b = 0; ! (a > 15); a = a + 1, b = b + 1 )
    {
      do_some_statement();
    }

### WITH-LOCAL-MEMORY statement

    WITH-LOCAL-MEMORY ({(var type size*)}*) statement*

`with-local-memory` declares new variable bindings on local memory by adding `__local` variable specifiers. It allows to declare array variables if dimensions are provided. A series of `statement`s are executed with these bindings.

Example:

    (with-local-memory ((a int 16)
                        (b float 16 16))
      (return))

Compiled:

    {
      __local int a[16];
      __local float b[16][16];
      return;
    }

### SET statement

    SET reference expression

`set` provides simple variable assignment. It accepts one of variable, structure and array references as `reference`.

Example:

    (set x 1.0)
    (set (float4-x y 1.0)
    (set (aref z 0) 1.0)

Compiled:

    x = 1.0;
    y.x = 1.0;
    z[0] = 1.0;

### PROGN statement

    PROGN statement*

`progn` evaluates `statement`s, in the order in which they are given.

Example:

    (progn
      (do-some-statements)
      (do-more-statements))

Compiled:

    do_some_statements();
    do_more_statements();

### RETURN statement

    RETURN [return-form]

`return` returns control, with `return-form` if supplied, from a kernel function.

Example:

    (return 0)

Compiled:

    return 0;

### Built in Functions

Implementation status of built in functions.

| Status | Functions |
|:------:|:----------|
| Yes    | Work-Item |
| Part   | Math |
| Part   | Integer |
| Part   | Common |
| Yes    | Geometric |
| No     | Relational |
| No     | Vector Data Load and Store |
| Yes    | Synchronization |
| Yes    | Explicit Memory Fence |
| No     | Async Copies from Global to Local Memory, Local to Global Memory, and Prefetch |
| Yes    | Atomic |
| Part   | Miscellaneous Vector |
| Yes    | printf |
| No     | Image Read and Write Functions |

## Author

* gos-k (mag4.elan@gmail.com)

C source generator is forked from [cl-cuda](https://github.com/takagi/cl-cuda).

## Copyright

2015 gos-k (mag4.elan@gmail.com)

### cl-cuda

Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the LLGPL License.
