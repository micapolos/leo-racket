#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/binding
  leo/compiler/binding-utils
  leo/compiler/expressions
  leo/compiler/expression
  leo/compiler/expression-resolve)

(define (binding-resolve-tuple ($binding : Binding) ($tuple : Tuple)) : (Option Expressions)
  (expression-resolve-tuple (binding-expression $binding) $tuple))

(check-equal?
  (option-app expressions-sexp-structure
    (binding-resolve-tuple binding-ab tuple-a))
  (pair `(ab a) structure-b))
