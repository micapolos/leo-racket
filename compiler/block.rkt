#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/binding
  leo/compiler/type-utils
  leo/compiler/identifier-utils
  leo/compiler/definition
  leo/compiler/syntax-utils)

(data block
  (syntax-stack : (Stackof Syntax))
  (binding-stack : (Stackof Binding)))

; -------------------------------------------------------------------------

(define (block-append-definition ($block : Block) ($definition : Definition)) : Block
  (define $syntax-stack (block-syntax-stack $block))
  (define $syntax-option (definition-syntax-option $definition))
  (block
    (push-option
      (block-syntax-stack $block)
      (definition-syntax-option $definition))
    (push-stack
      (block-binding-stack $block)
      (definition-binding-stack $definition))))

(bind $block
  (block-append-definition
    (block 
      (stack syntax-a syntax-b)
      (stack (binding type-a identifier-c) (binding type-b #f)))
    (definition 
      syntax-c
      (stack (binding type-c identifier-d) (binding type-d #f))))
  (check-equal?
    (map syntax->datum (block-syntax-stack $block))
    (stack `a `b `c))
  (check-equal?
    (map binding-type (block-binding-stack $block))
    (stack type-a type-b type-c type-d))
  (check-equal?
    (map binding-identifier-option (block-binding-stack $block))
    (stack identifier-c #f identifier-d #f)))

; -------------------------------------------------------------------------

(define (block-syntax-list ($block : Block)) : (Stackof Syntax)
  (reverse (block-syntax-stack $block)))

(check-equal?
  (map
    syntax->datum
    (block-syntax-list
      (block (stack syntax-a syntax-b) null)))
  (list `a `b))
