#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/testing
  leo/compiler/typed
  leo/compiler/type-utils
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-utils)

(define (expression-stack-syntax
  ($expression-stack : (Stackof Expression)))
  : Syntax
  (define $dynamic-expression-stack 
    (filter expression-is-dynamic? $expression-stack))
  (define $dynamic-syntax-stack
    (map expression-syntax $dynamic-expression-stack))
  (define $dynamic-length
    (length $dynamic-syntax-stack))
  (case $dynamic-length
    ((0) (make-syntax #f))
    ((1) (top $dynamic-syntax-stack))
    ((2) 
      (make-syntax
        `(cons 
          ,(pop-top $dynamic-syntax-stack) 
          ,(top $dynamic-syntax-stack))))
    (else 
      (make-syntax
        `(vector 
          ,@(reverse $dynamic-syntax-stack))))))

(check-equal?
  (syntax->datum (expression-stack-syntax null))
  #f)

(check-equal?
  (syntax->datum (expression-stack-syntax (stack static-expression-a)))
  #f)

(check-equal?
  (syntax->datum
    (expression-stack-syntax
      (stack dynamic-expression-a)))
  `a)

(check-equal?
  (syntax->datum
    (expression-stack-syntax
      (stack 
        dynamic-expression-a 
        static-expression-a)))
  `a)

(check-equal?
  (syntax->datum
    (expression-stack-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b)))
  `(cons a b))

(check-equal?
  (syntax->datum
    (expression-stack-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        static-expression-c)))
  `(cons a b))

(check-equal?
  (syntax->datum
    (expression-stack-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        dynamic-expression-c)))
  `(vector a b c))

(check-equal?
  (syntax->datum
    (expression-stack-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        dynamic-expression-c 
        static-expression-d)))
  `(vector a b c))
