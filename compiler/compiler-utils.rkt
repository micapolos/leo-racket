#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/compiler
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/compiler-plus-expressions
  leo/compiler/sexp-expression)

(define (compiler-with-ingredients ($compiler : Compiler) ($ingredients : Ingredients)) : Compiler
  (struct-copy compiler $compiler (ingredients $ingredients)))

(define (compiler-apply-type ($compiler : Compiler)) : Compiler
  (compiler-with-ingredients $compiler
    (ingredients-apply-type (compiler-ingredients $compiler))))

(define (compiler-apply-racket ($compiler : Compiler)) : Compiler 
  (compiler-with-ingredients $compiler
    (ingredients-apply-racket (compiler-ingredients $compiler))))

(define (compiler-apply-quote ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-plus-quoted-tuple $compiler
    (sexp-list-tuple
      (map syntax->datum $syntax-list))))

(define (compiler-apply-apply ($compiler : Compiler)) : Compiler
  (compiler-with-ingredients $compiler
    (scope-apply-ingredients
      (compiler-scope $compiler)
      (compiler-ingredients $compiler))))
