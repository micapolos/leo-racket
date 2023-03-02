#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/definition
  leo/compiler/definition-define-syntax
  leo/compiler/expression
  leo/compiler/expression-definition
  leo/compiler/type-utils
  leo/compiler/expression-utils
  leo/compiler/syntax-utils)

(define (expression-stack-define-syntax-stack
  ($expression-stack : (Stackof Expression)))
  : (Stackof Syntax)
  (define $dynamic-expression-stack
    (filter expression-is-dynamic? $expression-stack))
  (define $definition-stack 
    (map expression-definition $dynamic-expression-stack))
  (filter-false (map definition-define-syntax-option $definition-stack)))
