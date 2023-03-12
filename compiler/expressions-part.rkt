#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/compiler/expressions)

(define-type Expressions-Part (Stackof Expressions))

(define null-expressions-part null)

(define expressions-part stack)
