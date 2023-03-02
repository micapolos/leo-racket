#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/match
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/values
  leo/compiler/block
  leo/compiler/binding
  leo/compiler/syntax-utils
  leo/compiler/type-utils
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/expression-utils)

(define (values-block ($values : Values)) : Block
  (define $expression-stack (values-expression-stack $values))
  (define $type-stack (map expression-type $expression-stack))
  (define $identifier-option-stack (map type-generate-temporary-option $type-stack))
  (define $identifier-stack (filter-false $identifier-option-stack))
  (define $binding-stack (map binding $type-stack $identifier-option-stack))
  (define $dynamic-expression-stack (values-dynamic-expression-stack $values))
  (define $syntax-stack
    (case (length $dynamic-expression-stack)
      ((0) null)
      ((1) 
        (define $expression (car $dynamic-expression-stack))
        (define $type (expression-type $expression))
        (define $temporary (type-generate-temporary $type))
        (stack
          (make-syntax 
            `(define 
              ,$temporary
              ,(expression-syntax $expression)))))
      (else 
        (stack 
          (make-syntax 
            `(define-values 
              (,@(reverse $identifier-stack))
              ,(values-syntax $values)))))))
  (block $syntax-stack $binding-stack))

(bind $block
  (values-block 
    (values (stack dynamic-expression-a static-expression-b dynamic-expression-c)))
  (check-equal? 
    (match (map syntax->datum (block-syntax-stack $block))
      ((list (list `define-values (list tmp1 tmp2) `(values a c))) 
        ; TODO: Match tmp1 and tmp2 starts with a and c respectively
        #t)
      (else #f))
    #t)
  (check-equal?
    (map binding-type (block-binding-stack $block))
    (stack dynamic-type-a static-type-b dynamic-type-c))
  ; TODO: Match binding stack
)
