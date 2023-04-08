#lang leo/typed

(require
  leo/compiler/type)

(data binding
  (identifier-option : (Option Identifier))
  (type : Type))

(define-type Scope (Stackof Binding))

(define (scope-structure ($scope : Scope)) : Structure
  (map binding-type $scope))

(define (scope-identifier-stack ($scope : Scope)) : (Stackof Identifier)
  (filter-false (map binding-identifier-option $scope)))
