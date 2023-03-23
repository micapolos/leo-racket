#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/binder
  leo/compiler/ingredients)

(data program
  (entry-stack : (Stackof Entry))
  (ingredients : Ingredients))

(define null-program
  (program null null-ingredients))
