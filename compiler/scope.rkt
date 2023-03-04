#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/binding
  leo/compiler/binding-resolve
  leo/compiler/expression
  leo/compiler/package)

(define-type Scope (Stackof Binding))

(define null-scope null)

(define (scope-resolve-tuple 
  ($scope : Scope)
  ($tuple : Tuple))
  : (Option Package)
  (or 
    (and 
      (not (null? $scope)) 
      (binding-resolve-tuple (car $scope) $tuple))
    (scope-resolve-tuple (cdr $scope) $tuple)))
