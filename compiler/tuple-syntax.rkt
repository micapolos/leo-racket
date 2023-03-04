#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/typed
  leo/compiler/type-utils
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-utils)

(define (tuple-syntax
  ($tuple : Tuple))
  : Syntax
  (define $dynamic-tuple 
    (filter expression-dynamic? $tuple))
  (define $dynamic-syntax-stack
    (map expression-syntax $dynamic-tuple))
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
  (syntax->datum (tuple-syntax null))
  #f)

(check-equal?
  (syntax->datum (tuple-syntax (stack static-expression-a)))
  #f)

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack dynamic-expression-a)))
  `a)

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack 
        dynamic-expression-a 
        static-expression-a)))
  `a)

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b)))
  `(cons a b))

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        static-expression-c)))
  `(cons a b))

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        dynamic-expression-c)))
  `(vector a b c))

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        dynamic-expression-c 
        static-expression-d)))
  `(vector a b c))

; -----------------------------------------------------------------

(define (tuple-values-syntax-option 
  ($tuple : Tuple))
  : (Option Syntax)
  (define $dynamic-tuple (filter expression-dynamic? $tuple))
  (define $dynamic-syntax-stack (map expression-syntax $dynamic-tuple))
  (make-syntax
    (case (length $dynamic-syntax-stack)
      ((0) #f)
      ((1) (top $dynamic-syntax-stack))
      (else `(values ,@(reverse $dynamic-syntax-stack))))))

(check-equal?
  (option-map
    (tuple-values-syntax-option null)
    syntax->datum)
  #f)

(check-equal?
  (option-map
    (tuple-values-syntax-option
      (stack dynamic-expression-a))
    syntax->datum)
  `a)

(check-equal?
  (option-map
    (tuple-values-syntax-option
      (stack 
        dynamic-expression-a 
        static-expression-b 
        dynamic-expression-c))
    syntax->datum)
  `(values a c))
