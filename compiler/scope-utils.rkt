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
  leo/compiler/package)

(define (structure-generate-scope ($structure : Structure)) : Scope
  (map type-generate-binding $structure))

(define (scope-identifier-stack ($scope : Scope)) : (Stackof Identifier)
  (filter-false (map binding-identifier-option $scope)))

(define (scope-structure ($scope : Scope)) : Structure
  (map binding-type $scope))