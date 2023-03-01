#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define expression-a (expression syntax-a type-a))
(define expression-b (expression syntax-b type-b))
(define expression-c (expression syntax-c type-c))
(define expression-d (expression syntax-d type-d))

(define (expression-is-dynamic? ($expression : Expression)) : Boolean
  (type-is-dynamic? (expression-type $expression)))

(define (expression-stack-type-stack 
  ($expression-stack : (Stackof Expression)))
  : (Stackof Type)
  (map expression-type $expression-stack))

(define (expression-stack-syntax-stack
  ($expression-stack : (Stackof Expression)))
  : (Stackof Syntax)
  (map expression-syntax $expression-stack))

(define (expression-stack-dynamic-syntax-stack 
  ($expression-stack : (Stackof Expression)))
  : (Stackof Syntax)
  (expression-stack-syntax-stack
    (filter expression-is-dynamic? $expression-stack)))

(check-equal?
  (expression-stack-type-stack (stack expression-a expression-b))
  (stack type-a type-b))

(check-equal?
  (expression-stack-syntax-stack (stack expression-a expression-b))
  (stack syntax-a syntax-b))
