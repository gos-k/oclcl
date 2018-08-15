#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl-test.lang.compiler.compile-program
  (:use :cl :prove
        :oclcl.lang.type
        :oclcl.lang.program
        :oclcl.lang.compiler.compile-program))
(in-package :oclcl-test.lang.compiler.compile-program)

(plan nil)


;;;
;;; test COMPILE-PROGRAM funcition
;;;

(subtest "COMPILE-PROGRAM"

  (let ((program (make-program)))
    (program-define-memory program 'a :constant 1)
    (program-define-memory program 'b :global 1.0f0)
    (program-define-function program 'foo 'void '((x int*))
                            '((set (aref x 0) (bar 1))
                              (return)))
    (program-define-function program 'bar 'int '((x int)) '((return x)))
    (program-define-function program 'baz 'void '() '((return)))
    (is (compile-program program)
        "

/**
 *  Memory objects
 */

__constant int oclcl_test_lang_compiler_compile_program_a = 1;
__global float oclcl_test_lang_compiler_compile_program_b = 1.0f;


/**
 *  Kernel function prototypes
 */

__kernel void oclcl_test_lang_compiler_compile_program_baz();
int oclcl_test_lang_compiler_compile_program_bar(int x);
__kernel void oclcl_test_lang_compiler_compile_program_foo(__global int* x);


/**
 *  Kernel function definitions
 */

__kernel void oclcl_test_lang_compiler_compile_program_baz()
{
  return;
}

int oclcl_test_lang_compiler_compile_program_bar(int x)
{
  return x;
}

__kernel void oclcl_test_lang_compiler_compile_program_foo(__global int* x)
{
  x[0] = oclcl_test_lang_compiler_compile_program_bar(1);
  return;
}
"
        "basic case 1")))


(finalize)
