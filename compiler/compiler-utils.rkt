#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/compiler
  leo/compiler/package
  leo/compiler/package-utils
  leo/compiler/compiler-plus-expressions
  leo/compiler/sexp-expression)

(define (compiler-with-package ($compiler : Compiler) ($package : Package)) : Compiler
  (struct-copy compiler $compiler (package $package)))

(define (compiler-apply-type ($compiler : Compiler)) : Compiler
  (compiler-with-package $compiler
    (package-apply-type (compiler-package $compiler))))

(define (compiler-apply-compiled ($compiler : Compiler)) : Compiler 
  (compiler-with-package $compiler
    (package-apply-compiled (compiler-package $compiler))))

(define (compiler-apply-quote ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-plus-quoted-tuple $compiler 
    (sexp-list-tuple
      (map syntax->datum $syntax-list))))
