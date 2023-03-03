#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/expression
  leo/compiler/expression-utils)

(define (syntax-expression-option ($syntax : Syntax)) : (Option Expression)
  (define $syntax-e (syntax-e $syntax))
  (or
    (and (number? $syntax-e) (number-expression $syntax-e))
    (and (string? $syntax-e) (text-expression $syntax-e))))
