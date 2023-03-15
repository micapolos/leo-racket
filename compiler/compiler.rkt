#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/scope
  leo/compiler/ingredients)

(data compiler 
  (scope : Scope) 
  (ingredients : Ingredients))

(define null-compiler (compiler null-scope null-ingredients))
