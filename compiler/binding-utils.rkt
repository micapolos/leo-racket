#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/compiler/binding
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/typed
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/generate-temporary
  leo/compiler/expression-utils)

(define binding-ab (binding (arrow (structure type-a) (structure type-b)) `ab))
(define binding-cd (binding (arrow (structure type-c) (structure type-d)) `cd))

(define (binding-expression ($binding : Binding)) : Expression
  (define $symbol-option (binding-symbol-option $binding))
  (expression
    (or (option-app make-syntax $symbol-option) null-syntax)
    (binding-type $binding)))

(check-equal?
  (expression-sexp-type (binding-expression (binding type-a `b)))
  (pair `b type-a))

(define (type-generate-binding ($type : Type)) : Binding
  (binding $type (option-app syntax-e (type-generate-temporary-option $type))))
