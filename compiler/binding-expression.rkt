#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/binding
  leo/compiler/expression)

(define (binding-expression ($binding : Binding)) : Expression
  (expression
    (binding-identifier $binding)
    (binding-type $binding)))
