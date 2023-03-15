#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/compiler/expressions)

(define-type Ingredients (Stackof Expressions))

(define null-ingredients null)

(define ingredients : (-> Expressions * (Stackof Expressions)) stack)
