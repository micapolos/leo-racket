#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/type-utils
  leo/compiler/expression-utils
  leo/compiler/syntax-utils)

(define (expression-stack-values-syntax 
  ($expression-stack : (Stackof Expression)))
  : Syntax
  (define $dynamic-syntax-stack
    (expression-stack-dynamic-syntax-stack $expression-stack))
  (define $dynamic-length (length $dynamic-syntax-stack))
  (case $dynamic-length
    ((0) (make-syntax `(void)))
    ((1) (car $dynamic-syntax-stack))
    (else (make-syntax `(values ,@(reverse $dynamic-syntax-stack))))))

(check-equal?
  (syntax->datum (expression-stack-values-syntax null))
  `(void))

(check-equal?
  (syntax->datum 
    (expression-stack-values-syntax
      (stack 
        (expression syntax-a static-type-a))))
  `(void))

(check-equal?
  (syntax->datum 
    (expression-stack-values-syntax 
      (stack 
        (expression syntax-a dynamic-type-a))))
  `a)

(check-equal?
  (syntax->datum
    (expression-stack-values-syntax
      (stack
        (expression syntax-a dynamic-type-a)
        (expression syntax-b static-type-b)
        (expression syntax-c dynamic-type-c))))
  `(values a c))
