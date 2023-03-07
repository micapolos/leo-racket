#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/compiler
  leo/compiler/package
  leo/compiler/package-utils)

(define (compiler-with-package ($compiler : Compiler) ($package : Package)) : Compiler
  (struct-copy compiler $compiler (package $package)))

(define (compiler-apply-type ($compiler : Compiler)) : Compiler
  (compiler-with-package $compiler
    (package-apply-type (compiler-package $compiler))))

(define (compiler-apply-compiled ($compiler : Compiler)) : Compiler 
  (compiler-with-package $compiler
    (package-apply-compiled (compiler-package $compiler))))
