#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/expression
  leo/compiler/ingredients)

(data compiler 
  (tuple : Tuple) 
  (ingredients : Ingredients))

(define null-compiler (compiler null-tuple null-ingredients))
