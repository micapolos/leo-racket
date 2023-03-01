#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/definition
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/generate-temporary
  leo/compiler/sourced
  leo/compiler/syntax-utils
  leo/compiler/typed
  leo/compiler/srcloc
  leo/compiler/type-utils
  leo/typed/testing
  leo/typed/option
  leo/typed/base)

(define (expression-definition ($expression : Expression)) : Definition
  (define $type (expression-type $expression))
  (define $syntax (expression-syntax $expression))
  (define $temporary
    (and 
      (type-is-dynamic? $type) 
      (type-generate-temporary $type)))
  (define $definition-expression
    (expression (or $temporary (make-syntax #f)) $type))
  (define $definition-syntax-option
    (and 
      $temporary
      (make-syntax `(define ,$temporary ,$syntax))))
  (definition $definition-expression $definition-syntax-option))

(bind $definition
  (expression-definition expression-a)
  (define $expression (definition-expression $definition))
  (define $tmp-symbol (syntax-e (expression-syntax $expression)))
  (check-equal? (expression-type $expression) type-a)
  (check-equal? 
    (syntax-sourced (expression-syntax $expression))
    (sourced $tmp-symbol empty-srcloc))
  (check-equal?
    (option-map
      (definition-syntax-option $definition) 
      syntax-sourced)
    (sourced `(define ,$tmp-symbol a) empty-srcloc)))

(bind $definition
  (expression-definition (expression syntax-a static-type-a))
  (define $expression (definition-expression $definition))
  (define $tmp-symbol (syntax-e (expression-syntax $expression)))
  (check-equal? (expression-type $expression) static-type-a)
  (check-equal? 
    (syntax-sourced (expression-syntax $expression))
    (sourced $tmp-symbol empty-srcloc))
  (check-equal? (definition-syntax-option $definition) #f))
