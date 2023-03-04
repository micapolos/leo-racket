#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/type-utils
  leo/compiler/typed)

(define (syntax-expression-option ($syntax : Syntax)) : (Option Expression)
  (define $syntax-e (syntax-e $syntax))
  (or
    (and (number? $syntax-e) (number-expression $syntax-e))
    (and (string? $syntax-e) (text-expression $syntax-e))))

(check-equal?
  (option-map 
    (syntax-expression-option (make-syntax 1)) 
    expression-sexp-type)
  (pair 1 number-type))

(check-equal?
  (option-map 
    (syntax-expression-option (make-syntax "foo")) 
    expression-sexp-type)
  (pair "foo" text-type))
