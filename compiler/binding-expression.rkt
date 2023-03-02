#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/testing
  leo/compiler/binding
  leo/compiler/type-utils
  leo/compiler/typed
  leo/compiler/expression
  leo/compiler/expression-utils)

(define (binding-expression ($binding : Binding)) : Expression
  (expression
    (binding-identifier $binding)
    (binding-type $binding)))

(check-equal?
  (expression-typed-datum
    (binding-expression
      (binding type-a #`b)))
  (typed `b type-a))
