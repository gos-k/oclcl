#|
  This file is a part of oclcl project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
                2015-2025 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage oclcl.tests.lang.compiler.compile-program
  (:use :cl :rove
        :oclcl.tests.utils
        :oclcl.lang.type
        :oclcl.lang.program
        :oclcl.lang.compiler.compile-program))
(in-package :oclcl.tests.lang.compiler.compile-program)

;;;
;;; test COMPILE-PROGRAM funcition
;;;

(deftest compile-program
  (testing "empty env"
    (let ((program (make-program)))
      (is (compile-program program) "")))

  (let ((program (make-program)))
    (program-define-define program 'charlie-delta 1.0)
    (program-define-define program 'echo-foxtrot '(+ 10 20))
    (is (compile-program program)
        "/**
 *  Define
 */

#define CHARLIE_DELTA 1.0f
#define ECHO_FOXTROT (10 + 20)
"))

  (let ((program (make-program)))
    (program-define-memory program 'a :constant 1)
    (program-define-memory program 'b :global 1.0f0)
    (is (compile-program program)
        "/**
 *  Memory objects
 */

__constant int oclcl_tests_lang_compiler_compile_program_a = 1;
__global float oclcl_tests_lang_compiler_compile_program_b = 1.0f;
"))

  (let ((program (make-program)))
    (program-define-function program 'foo 'void '((x int*))
                             '((set (aref x 0) (bar 1))
                               (return)))
    (program-define-function program 'bar 'int '((x int)) '((return x)))
    (program-define-function program 'baz 'void '() '((return)))
    (is (compile-program program)
        "/**
 *  Kernel function prototypes
 */

__kernel void oclcl_tests_lang_compiler_compile_program_foo(__global int* x);
int oclcl_tests_lang_compiler_compile_program_bar(int x);
__kernel void oclcl_tests_lang_compiler_compile_program_baz();


/**
 *  Kernel function definitions
 */

__kernel void oclcl_tests_lang_compiler_compile_program_foo(__global int* x)
{
  x[0] = oclcl_tests_lang_compiler_compile_program_bar(1);
  return;
}

int oclcl_tests_lang_compiler_compile_program_bar(int x)
{
  return x;
}

__kernel void oclcl_tests_lang_compiler_compile_program_baz()
{
  return;
}
"))

  (let ((program (make-program)))
    (program-define-memory program 'a :constant 1)
    (program-define-memory program 'b :global 1.0f0)
    (program-define-function program 'foo 'void '((x int*))
                             '((set (aref x 0) (bar 1))
                               (return)))
    (program-define-function program 'bar 'int '((x int)) '((return x)))
    (program-define-function program 'baz 'void '() '((return)))
    (program-define-define program 'charlie-delta 1.0)
    (program-define-define program 'echo-foxtrot '(+ 10 20))
    (is (compile-program program)
        "/**
 *  Define
 */

#define CHARLIE_DELTA 1.0f
#define ECHO_FOXTROT (10 + 20)


/**
 *  Memory objects
 */

__constant int oclcl_tests_lang_compiler_compile_program_a = 1;
__global float oclcl_tests_lang_compiler_compile_program_b = 1.0f;


/**
 *  Kernel function prototypes
 */

__kernel void oclcl_tests_lang_compiler_compile_program_foo(__global int* x);
int oclcl_tests_lang_compiler_compile_program_bar(int x);
__kernel void oclcl_tests_lang_compiler_compile_program_baz();


/**
 *  Kernel function definitions
 */

__kernel void oclcl_tests_lang_compiler_compile_program_foo(__global int* x)
{
  x[0] = oclcl_tests_lang_compiler_compile_program_bar(1);
  return;
}

int oclcl_tests_lang_compiler_compile_program_bar(int x)
{
  return x;
}

__kernel void oclcl_tests_lang_compiler_compile_program_baz()
{
  return;
}
")))
