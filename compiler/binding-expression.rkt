#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/binding
  leo/compiler/type-utils
  leo/compiler/typed
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-utils)

(define (binding-expression ($binding : Binding)) : Expression
  (define $identifier-option (binding-identifier-option $binding))
  (expression
    (or $identifier-option empty-syntax)
    (binding-type $binding)))

(check-equal?
  (expression-sexp-type
    (binding-expression
      (binding type-a #`b)))
  (pair `b type-a))
