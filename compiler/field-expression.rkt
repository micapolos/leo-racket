#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expression-stack-syntax
  leo/compiler/type
  leo/compiler/typed
  leo/compiler/type-utils)

(define (field-expression 
  ($symbol : Symbol) 
  ($expression-stack : (Stackof Expression)))
  : Expression
  (expression
    (expression-stack-syntax $expression-stack)
    (field $symbol (map expression-type $expression-stack))))

(check-equal?
  (expression-typed-datum
    (field-expression `point
      (stack 
        dynamic-expression-a 
        static-expression-b 
        dynamic-expression-c)))
  (typed 
    `(cons a c) 
    (field `point 
      (stack 
        dynamic-type-a 
        static-type-b 
        dynamic-type-c))))