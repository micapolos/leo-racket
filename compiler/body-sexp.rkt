#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/testing
  leo/compiler/body
  leo/compiler/syntax-utils)

(define (body-sexp ($body : Body)) : Sexp
  `(body ,@(reverse (map syntax->datum $body))))

(check-equal?
  (body-sexp (stack syntax-a syntax-b))
  `(body a b))
