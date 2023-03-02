#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/testing
  leo/compiler/binding
  leo/compiler/definition
  leo/compiler/syntax-utils)

(struct block (
  (syntax-stack : (Stackof Syntax))
  (binding-stack : (Stackof Binding)))
  #:transparent
  #:type-name Block)

(define (block-append-definition ($block : Block) ($definition : Definition)) : Block
  (define $syntax-stack (block-syntax-stack $block))
  (define $syntax-option (definition-syntax-option $definition))
  (block
    (or 
      (and $syntax-option (push $syntax-stack $syntax-option))
      $syntax-stack)
    (push-stack
      (block-binding-stack $block)
      (definition-binding-stack $definition))))

(define (block-syntax-list ($block : Block)) : (Stackof Syntax)
  (reverse (block-syntax-stack $block)))

(check-equal?
  (map
    syntax->datum
    (block-syntax-list
      (block (stack syntax-a syntax-b) null)))
  (list `a `b))
