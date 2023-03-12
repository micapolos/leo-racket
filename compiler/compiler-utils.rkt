#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/compiler
  leo/compiler/expressions-part
  leo/compiler/expressions-part-utils
  leo/compiler/compiler-plus-expressions
  leo/compiler/sexp-expression)

(define (compiler-with-expressions-part ($compiler : Compiler) ($expressions-part : Expressions-Part)) : Compiler
  (struct-copy compiler $compiler (expressions-part $expressions-part)))

(define (compiler-apply-type ($compiler : Compiler)) : Compiler
  (compiler-with-expressions-part $compiler
    (expressions-part-apply-type (compiler-expressions-part $compiler))))

(define (compiler-apply-racket ($compiler : Compiler)) : Compiler 
  (compiler-with-expressions-part $compiler
    (expressions-part-apply-racket (compiler-expressions-part $compiler))))

(define (compiler-apply-quote ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-plus-quoted-tuple $compiler
    (sexp-list-tuple
      (map syntax->datum $syntax-list))))

(define (compiler-apply-apply ($compiler : Compiler)) : Compiler
  (compiler-with-expressions-part $compiler
    (scope-apply-expressions-part
      (compiler-scope $compiler)
      (compiler-expressions-part $compiler))))
