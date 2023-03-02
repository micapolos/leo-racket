#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/list
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-utils)

(data values (expression-stack : (Stackof Expression)))

(define null-values (values null))

(define (values-append ($lhs-values : Values) ($rhs-values : Values)) : Values
  (values 
    (push-stack
      (values-expression-stack $lhs-values)
      (values-expression-stack $rhs-values))))

(define (values-dynamic-expression-stack ($values : Values)) : (Stackof Expression)
  (filter expression-dynamic? (values-expression-stack $values)))

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

(define (values-syntax-option ($values : Values)) : (Option Syntax)
  (define $syntax-list (values-syntax-list $values))
  (make-syntax
    (case (length $syntax-list)
      ((0) #f)
      ((1) (car $syntax-list))
      (else `(values ,@$syntax-list)))))

(check-equal?
  (option-map
    (values-syntax-option (values null))
    syntax->datum)
  #f)

(check-equal?
  (option-map
    (values-syntax-option
      (values (stack dynamic-expression-a)))
    syntax->datum)
  `a)

(check-equal?
  (option-map
    (values-syntax-option
      (values
        (stack 
          dynamic-expression-a 
          static-expression-b 
          dynamic-expression-c)))
    syntax->datum)
  `(values a c))

; -----------------------------------------------------------------------

(define (values-partition-identifiers ($values : Values)) : (Pairof Values Values)
  (define $expression-stack (values-expression-stack $values))
  (define-values ($identifier-expression-stack $not-identifier-expression-stack) 
    (partition expression-identifier? $expression-stack))
  (cons
    (values $identifier-expression-stack) 
    (values $not-identifier-expression-stack)))
