#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/binding
  leo/compiler/definition
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/generate-temporary
  leo/compiler/sourced
  leo/compiler/syntax-utils
  leo/compiler/typed
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
  (define $binding 
    (binding $type (or $temporary (make-syntax empty-srcloc #f))))
  (define $definition-syntax-option
    (and 
      $temporary
      (make-syntax empty-srcloc `(define ,$temporary ,$syntax))))
  (definition $binding $definition-syntax-option))

(bind $definition
  (expression-definition expression-a)
  (define $binding (definition-binding $definition))
  (define $tmp-symbol (syntax-e (binding-syntax $binding)))
  (check-equal? (binding-type $binding) type-a)
  (check-equal? 
    (syntax-sourced (binding-syntax $binding))
    (sourced $tmp-symbol empty-srcloc))
  (check-equal?
    (option-map
      (definition-syntax-option $definition) 
      syntax-sourced)
    (sourced `(define ,$tmp-symbol a) empty-srcloc)))

(bind $definition
  (expression-definition (expression syntax-a static-type-a))
  (define $binding (definition-binding $definition))
  (define $tmp-symbol (syntax-e (binding-syntax $binding)))
  (check-equal? (binding-type $binding) static-type-a)
  (check-equal? 
    (syntax-sourced (binding-syntax $binding))
    (sourced $tmp-symbol empty-srcloc))
  (check-equal? (definition-syntax-option $definition) #f))
