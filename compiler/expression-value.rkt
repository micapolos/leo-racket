#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/environment
  leo/typed/stack
  leo/typed/testing
  leo/compiler/runtime-environment
  leo/compiler/expression
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define (expression-value ($expression : Expression)) : Value
  (define $syntax (expression-syntax $expression))
  (define $type (expression-type $expression))
  (define $sexp (syntax->datum $syntax))
  (define $value-any (environment-eval runtime-environment $sexp))
  (value $value-any $type))

(define (tuple-value-stack ($tuple : Tuple)) : (Stackof Value)
  (map expression-value $tuple))

(check-equal?
  (expression-value
    (expression 
      (make-syntax `(string-append "Hello, " "world!"))
      text-type))
  (value "Hello, world!" text-type))
