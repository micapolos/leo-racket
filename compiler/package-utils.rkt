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

(define (symbol-package-expression ($symbol : Symbol) ($package : Package)) : Expression
  (define $expressions-option (package-expressions-option $package))
  (define $tuple (package-tuple $package))
  (or
    (and $expressions-option 
      (expressions-do-expression $expressions-option 
        (lambda (($scope : Scope))
          (expression
            (tuple-syntax (push-stack (scope-tuple $scope) $tuple))
            (field $symbol (tuple-structure $tuple))))))
    (expression
      (tuple-syntax $tuple)
      (field $symbol (tuple-structure $tuple)))))
