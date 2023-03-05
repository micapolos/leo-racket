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
  leo/compiler/package
  leo/compiler/expression
  leo/compiler/expression-resolve)

(define (binding-resolve-tuple ($binding : Binding) ($tuple : Tuple)) : (Option Package)
  (expression-resolve-tuple (binding-expression $binding) $tuple))

(check-equal?
  (option-app package-sexp-structure
    (binding-resolve-tuple
      (binding (arrow (structure type-a) (structure type-b)) `fn)
      (tuple expression-a)))
  (pair `(fn a) (structure type-b)))
