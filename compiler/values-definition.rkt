#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/match
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/values
  leo/compiler/definition
  leo/compiler/binding
  leo/compiler/syntax-utils
  leo/compiler/type-utils
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/expression-utils)

(define (values-definition ($values : Values)) : Definition
  (define $expression-stack (values-expression-stack $values))
  (define $type-stack (map expression-type $expression-stack))
  (define $identifier-option-stack (map type-generate-temporary-option $type-stack))
  (define $identifier-stack (filter-false $identifier-option-stack))
  (define $binding-stack (map binding $type-stack $identifier-option-stack))
  (define $dynamic-expression-stack (values-dynamic-expression-stack $values))
  (define $syntax-option
    (case (length $dynamic-expression-stack)
      ((0) #f)
      ((1) 
        (define $expression (car $dynamic-expression-stack))
        (define $type (expression-type $expression))
        (define $temporary (type-generate-temporary $type))
        (make-syntax 
          `(define 
            ,$temporary
            ,(expression-syntax $expression))))
      (else 
        (make-syntax 
          `(define-values 
            (,@(reverse $identifier-stack))
            ,(values-syntax $values))))))
  (definition $syntax-option $binding-stack))

(parameterize ((tmp-temporaries? #t))
  (bind $definition
    (values-definition 
      (values (stack dynamic-expression-a static-expression-b dynamic-expression-c)))
    (check-equal?
      (option-map (definition-syntax-option $definition) syntax->datum)
      `(define-values (tmp-a tmp-c) (values a c)))
    (check-equal?
      (map binding-type (definition-binding-stack $definition))
      (stack dynamic-type-a static-type-b dynamic-type-c))
    (check-equal?
      (map
        (lambda (($identifier-option : (Option Identifier)))
          (and $identifier-option (syntax->datum $identifier-option)))
        (map binding-identifier-option (definition-binding-stack $definition)))
      (stack `tmp-a #f `tmp-c))))
