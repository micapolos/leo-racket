#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/compiler/scope
  leo/compiler/scope-resolve
  leo/compiler/expressions-utils
  leo/compiler/expression-resolve
  leo/compiler/expression
  (for-syntax racket/base))

(data tuple-compiler
  (scope : Scope)
  (tuple : Tuple))

(define null-tuple-compiler : Tuple-Compiler
  (tuple-compiler null-scope null-tuple))

(define (tuple-compiler-append-expression
  ($tuple-compiler : Tuple-Compiler)
  ($expression : Expression))
  : Tuple-Compiler
  (define $scope (tuple-compiler-scope $tuple-compiler))
  (define $tuple (tuple-compiler-tuple $tuple-compiler))
  (define $new-tuple (push $tuple $expression))
  (tuple-compiler
    $scope
    (or
      (option-app expressions-tuple
        (or
          (tuple-resolve $new-tuple)
          (scope-resolve-tuple $scope $new-tuple)))
      $new-tuple)))
