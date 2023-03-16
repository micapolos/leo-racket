#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/binding)

(define-type Scope (Stackof Binding))

(define null-scope null)

(define scope stack)

(define (scope-sexp ($scope : Scope)) : Sexp
  `(scope
    ,(reverse (map binding-sexp $scope))))
