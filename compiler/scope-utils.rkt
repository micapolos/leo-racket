#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/binding
  leo/compiler/binding-utils
  leo/compiler/scope
  leo/compiler/type
  leo/compiler/expression
  leo/compiler/expressions)

(define (structure-generate-scope ($structure : Structure)) : Scope
  (map type-generate-binding $structure))

(define (scope-symbol-stack ($scope : Scope)) : (Stackof Symbol)
  (filter-false (map binding-symbol-option $scope)))

(define (scope-structure ($scope : Scope)) : Structure
  (map binding-type $scope))
