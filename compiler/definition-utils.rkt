#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/binding
  leo/compiler/definition
  leo/compiler/generate-temporary
  leo/compiler/sourced
  leo/compiler/syntax-utils
  leo/compiler/typed
  leo/compiler/typed-syntax
  leo/compiler/type-utils
  leo/typed/testing
  leo/typed/option
  leo/typed/base)

(define (typed-syntax-definition
  ($typed-syntax : Typed-Syntax) 
  ($srcloc : srcloc)) : Definition
  (define $type (typed-type $typed-syntax))
  (define $syntax (typed-value $typed-syntax))
  (define $temporary
    (and 
      (type-is-dynamic? $type) 
      (type-generate-temporary $type)))
  (define $binding 
    (binding $type (or $temporary (make-syntax empty-srcloc #f))))
  (define $definition-syntax-option
    (and 
      $temporary
      (make-syntax $srcloc `(define ,$temporary ,$syntax))))
  (definition $binding $definition-syntax-option))

(bind $definition
  (typed-syntax-definition typed-syntax-a srcloc-b)
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
    (sourced `(define ,$tmp-symbol a) srcloc-b)))

(bind $definition
  (typed-syntax-definition (typed syntax-a static-type-a) srcloc-b)
  (define $binding (definition-binding $definition))
  (define $tmp-symbol (syntax-e (binding-syntax $binding)))
  (check-equal? (binding-type $binding) static-type-a)
  (check-equal? 
    (syntax-sourced (binding-syntax $binding))
    (sourced $tmp-symbol empty-srcloc))
  (check-equal? (definition-syntax-option $definition) #f))
