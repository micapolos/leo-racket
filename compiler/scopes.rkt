#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/compiler/scope)

(data scopes (stack : (Stackof Scope)))

(define null-scopes (scopes null))

(define (scopes-push ($scopes : Scopes) ($scope : Scope)) : Scopes
  (cond
    ((and (null? $scopes) (null? $scope)) null-scopes)
    (else (scopes (push (scopes-stack $scopes) $scope)))))

(define (scopes-pairof-scopes-scope ($scopes : Scopes)) : (Pairof Scopes Scope)
  (define $scope-stack (scopes-stack $scopes))
  (cond
    ((null? $scope-stack) (pair null-scopes null-scope))
    (else (pair (scopes (pop $scope-stack)) (top $scope-stack)))))

(define (scopes-in-value-scope ($scopes : Scopes) ($fn : (-> Scope Scope))) : Scopes
  (define $pairof-scopes-scope (scopes-pairof-scopes-scope $scopes))
  (scopes-push (car $pairof-scopes-scope) ($fn (cdr $pairof-scopes-scope))))

(define (scopes-in-type-scopes ($scopes : Scopes) ($fn : (-> Scopes Scopes))) : Scopes
  (define $pairof-scopes-scope (scopes-pairof-scopes-scope $scopes))
  (scopes-push ($fn (car $pairof-scopes-scope)) (cdr $pairof-scopes-scope)))
