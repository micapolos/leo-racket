#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/expression
  leo/compiler/syntax-utils
  leo/compiler/type-utils)

(define expression-a (expression syntax-a type-a))
(define expression-b (expression syntax-b type-b))
(define expression-c (expression syntax-c type-c))
(define expression-d (expression syntax-d type-d))

(define (expression-is-dynamic? ($expression : Expression)) : Boolean
  (type-is-dynamic? (expression-type $expression)))
