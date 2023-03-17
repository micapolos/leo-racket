#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/scope
  leo/compiler/binding
  leo/compiler/binding-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/binding-resolve
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions)

(define (scope-resolve-first-fn ($scope : Scope) ($fn : (-> Expression (Option Expressions)))) : (Option Expressions)
  (and
    (not (null? $scope))
    (or
      (binding-resolve-fn (top $scope) $fn)
      (scope-resolve-first-fn (pop $scope) $fn))))

(define (scope-resolve-tuple 
  ($scope : Scope)
  ($tuple : Tuple))
  : (Option Expressions)
  (and
    (not (null? $scope))
    (or
      (binding-resolve-tuple (car $scope) $tuple)
      (scope-resolve-tuple (cdr $scope) $tuple))))

(check-equal?
  (option-app expressions-sexp-structure
    (scope-resolve-tuple (scope binding-ab binding-cd) tuple-a))
  (pair `(#%app ab a) structure-b))

(check-equal?
  (option-app expressions-sexp-structure
    (scope-resolve-tuple (scope binding-ab binding-cd) tuple-c))
  (pair `(#%app cd c) structure-d))

(check-equal?
  (scope-resolve-tuple (scope binding-ab binding-cd) tuple-b)
  #f)
