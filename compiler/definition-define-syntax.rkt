#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/definition
  leo/compiler/expression
  leo/compiler/type-utils
  leo/compiler/expression-utils
  leo/compiler/syntax-utils)

(define (definition-define-syntax-option 
  ($definition : Definition)) 
  : (Option Syntax)
  (option-bind 
    (definition-syntax-option $definition) 
    $syntax-option
    (define $expression (definition-expression $definition))
    (make-syntax 
      `(define 
        ,$syntax-option
        ,(expression-syntax $expression)))))
