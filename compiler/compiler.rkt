#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/ingredients
  leo/compiler/ingredients-sexp)

(data compiler 
  (tuple : Tuple) 
  (ingredients : Ingredients))

(define null-compiler (compiler null-tuple null-ingredients))

(define (compiler-sexp ($compiler : Compiler)) : Sexp
  `(compiler
    ,(tuple-sexp (compiler-tuple $compiler))
    ,(ingredients-sexp (compiler-ingredients $compiler))))
