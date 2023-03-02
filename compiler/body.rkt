#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/identifier-utils
  leo/compiler/expression-utils
  leo/compiler/type-utils
  leo/compiler/binding
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

; --------------------------------------------------------------

(define (body-syntax-list ($body : Body)) : (Listof Syntax)
  (define $block (body-block $body))
  (define $values (body-values $body))
  (append
    (block-syntax-list $block)
    (filter-false (list (values-syntax-option $values)))))

(check-equal?
  (map syntax->datum
    (body-syntax-list 
      (body
         (block 
            (stack (make-syntax `(define pi 3.14)))
            (stack (binding type-c identifier-c)))
         (values (stack expression-c expression-d)))))
  `((define pi 3.14) (values c d)))
