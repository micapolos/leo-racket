#lang leo/typed

(require 
  leo/compiler/expression
  leo/compiler/type-utils
  leo/compiler/expression-utils
  leo/compiler/syntax-utils)

(define (tuple-values-syntax 
  ($tuple : Tuple))
  : (Option Syntax)
  (define $dynamic-syntax-stack
    (tuple-syntax-stack $tuple))
  (define $dynamic-length (length $dynamic-syntax-stack))
  (case $dynamic-length
    ((0) #f)
    ((1) (car $dynamic-syntax-stack))
    (else (make-syntax `(values ,@(reverse $dynamic-syntax-stack))))))

(check-equal?
  (syntax->datum (tuple-values-syntax null))
  `null)

(check-equal?
  (syntax->datum 
    (tuple-values-syntax
      (stack 
        (expression syntax-a static-type-a))))
  `null)

(check-equal?
  (syntax->datum 
    (tuple-values-syntax 
      (stack 
        (expression syntax-a dynamic-type-a))))
  `a)

(check-equal?
  (syntax->datum
    (tuple-values-syntax
      (stack
        (expression syntax-a dynamic-type-a)
        (expression syntax-b static-type-b)
        (expression syntax-c dynamic-type-c))))
  `(values a c))
