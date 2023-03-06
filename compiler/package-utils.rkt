#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/package
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expression-resolve)

(define (package-push-expression ($package : Package) ($expression : Expression)) : Package
  (package 
    (package-expressions-option $package)
    (push (package-tuple $package) $expression)))

(define (package-push-tuple ($package : Package) ($tuple : Tuple)) : Package
  (package 
    (package-expressions-option $package)
    (push-stack (package-tuple $package) $tuple)))

; -------------------------------------------------------------------------------

(define (package-make ($package : Package) ($fn : (-> Tuple Expression))) : Expression
  (define $expressions-option (package-expressions-option $package))
  (define $tuple (package-tuple $package))
  (or
    (and $expressions-option 
      (expressions-do-expression $expressions-option 
        (lambda (($scope : Scope))
          ($fn (push-stack (scope-tuple $scope) $tuple)))))
    ($fn $tuple)))

; --------------------------------------------------------------------------------

(define (symbol-package-expression ($symbol : Symbol) ($package : Package)) : Expression
  (package-make $package
    (lambda (($tuple : Tuple))
      (expression
        (tuple-syntax $tuple)
        (field $symbol (tuple-structure $tuple))))))

; -------------------------------------------------------------------------------

(define (package-do ($package : Package) ($fn : (-> Scope Expressions))) : Expressions
  (define $expressions-option (package-expressions-option $package))
  (define $tuple (package-tuple $package))
  (or
    (and $expressions-option
      (expressions-do $expressions-option
        (lambda (($scope : Scope))
          (tuple-do $tuple
            (lambda (($tuple-scope : Scope))
              ($fn (push-stack $scope $tuple-scope)))))))
    (tuple-do $tuple $fn)))

(check-equal?
  (expressions-sexp
    (package-do
      (package #f (tuple))
      (lambda (($scope : Scope))
        (make-expressions #`result (scope-structure $scope)))))
  `(expressions #f (structure)))

(check-equal?
  (expressions-sexp
    (package-do
      (package
        #f
        (tuple dynamic-expression-c dynamic-expression-d))
      (lambda (($scope : Scope))
        (make-expressions #`result (scope-structure $scope)))))
  `(expressions
    (let-values (((tmp-c tmp-d) (values c d))) result)
    (structure (racket c) (racket d))))

(check-equal?
  (expressions-sexp
    (package-do
      (package
        (expressions #`exprs (structure dynamic-type-a dynamic-type-b))
        (tuple dynamic-expression-c dynamic-expression-d))
      (lambda (($scope : Scope))
        (make-expressions #`result (scope-structure $scope)))))
  `(expressions
    (let-values (((tmp-a tmp-b) exprs))
      (let-values (((tmp-c tmp-d) (values c d)))
        result))
    (structure (racket a) (racket b) (racket c) (racket d))))

