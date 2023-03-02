#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/syntax-utils)

(struct block (
  (syntax-stack : (Stackof Syntax))
  (type-stack : (Stackof Type)))
  #:transparent
  #:type-name Block)

(define (block-syntax-list ($block : Block)) : (Stackof Syntax)
  (reverse (block-syntax-stack $block)))

(check-equal?
  (map
    syntax->datum
    (block-syntax-list
      (block (stack syntax-a syntax-b) null)))
  (list `a `b))
