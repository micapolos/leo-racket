#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-utils)

(struct values (
  (expression-stack : (Stackof Expression)))
  #:transparent
  #:type-name Values)

(define null-values (values null))

(define (values-dynamic-expression-stack ($values : Values)) : (Stackof Expression)
  (filter expression-is-dynamic? (values-expression-stack $values)))

(define (values-syntax-list ($values : Values)) : (Listof Syntax)
  (reverse (map expression-syntax 
    (values-dynamic-expression-stack $values))))

(check-equal?
  (map syntax->datum
    (values-syntax-list
      (values
        (stack 
          dynamic-expression-a 
          static-expression-b 
          dynamic-expression-c))))
  (list `a `c))

; -----------------------------------------------------------------

(define (values-syntax ($values : Values)) : Syntax
  (define $syntax-list (values-syntax-list $values))
  (make-syntax
    (case (length $syntax-list)
      ((0) `(void))
      ((1) (car $syntax-list))
      (else `(values ,@$syntax-list)))))

(check-equal?
  (syntax->datum
    (values-syntax
      (values null)))
  `(void))

(check-equal?
  (syntax->datum
    (values-syntax
      (values (stack dynamic-expression-a))))
  `a)

(check-equal?
  (syntax->datum
    (values-syntax
      (values
        (stack 
          dynamic-expression-a 
          static-expression-b 
          dynamic-expression-c))))
  `(values a c))
