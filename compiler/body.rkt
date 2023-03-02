#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/generate-temporary
  leo/compiler/identifier-utils
  leo/compiler/expression
  leo/compiler/type-utils
  leo/compiler/binding
  leo/compiler/block
  leo/compiler/values
  leo/compiler/values-definition)

(data body
  (block : Block)
  (values : Values))

; --------------------------------------------------------------

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
          (stack 
            (make-syntax `(define-values (zero pi) (values 0 3.14)))
            (make-syntax `(define (plus a b) (+ a b))))
          (stack (binding type-c identifier-c)))
        (values 
          (stack 
            (expression (make-syntax `pi) type-a)
            (expression (make-syntax `(plus pi 2)) type-b))))))
  `(
    (define-values (zero pi) (values 0 3.14))
    (define (plus a b) (+ a b)) 
    (values pi (plus pi 2))))

(parameterize ((tmp-temporaries? #t))
  (check-equal?
    (map syntax->datum
      (block-syntax-list 
        (body-as-block
          (body
            (block 
              (stack 
                (make-syntax `(define-values (zero pi) (values 0 3.14)))
                (make-syntax `(define (plus a b) (+ a b))))
              (stack (binding type-c identifier-c)))
           (values 
              (stack 
                (expression (make-syntax `pi) type-a)
                (expression (make-syntax `(plus pi 2)) type-b)))))))
    `(
      (define-values (zero pi) (values 0 3.14))
      (define (plus a b) (+ a b)) 
      (define-values (tmp-a tmp-b) (values pi (plus pi 2))))))

; --------------------------------------------------------------------

(define (body-append ($lhs-body : Body) ($rhs-body : Body)) : Body
  (define $lhs-block (body-block $lhs-body))
  (define $lhs-syntax-stack (block-syntax-stack $lhs-block))
  (define $lhs-binding-stack (block-binding-stack $lhs-block))
  (define $lhs-values (body-values $lhs-body))
  (define $rhs-block (body-block $rhs-body))
  (define $rhs-syntax-stack (block-syntax-stack $rhs-block))
  (define $rhs-binding-stack (block-binding-stack $rhs-block))
  (define $rhs-values (body-values $rhs-body))
  (body
    (block
      (push-stack $lhs-syntax-stack $rhs-syntax-stack)
      (push-stack $lhs-binding-stack $rhs-binding-stack))
    (values-append $lhs-values $rhs-values)))
