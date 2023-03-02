#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/compiler/block
  leo/compiler/values
  leo/compiler/values-definition)

(struct body (
  (block : Block)
  (values : Values))
  #:transparent
  #:type-name Body)

(define (body-as-block ($body : Body)) : Block
  (block-append-definition 
    (body-block $body) 
    (values-definition (body-values $body))))
