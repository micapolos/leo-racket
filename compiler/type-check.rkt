#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/type
  leo/typed/stack)

(define (type-check? ($actual : Type) ($expected : Type)) : Boolean
  (equal? $actual $expected))

(define (type-stack-check? 
  ($actual : (Stackof Type))
  ($expected : (Stackof Type))) 
  : Boolean
  (and 
    (= (length $actual) (length $expected))
    (andmap type-check? $actual $expected)))
