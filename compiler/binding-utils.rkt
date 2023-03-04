#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/binding
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/typed
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/generate-temporary
  leo/compiler/expression-utils)

(define (binding-expression ($binding : Binding)) : Expression
  (define $identifier-option (binding-identifier-option $binding))
  (expression
    (or $identifier-option null-syntax)
    (binding-type $binding)))

(check-equal?
  (expression-sexp-type
    (binding-expression
      (binding type-a #`b)))
  (pair `b type-a))

(define (type-generate-binding ($type : Type)) : Binding
  (binding $type (type-generate-temporary-option $type)))
