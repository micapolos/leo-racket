#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/generate-temporary)

(data binding
  (identifier : Identifier)
  (syntax : Syntax))

(define-type Scope (Stackof Binding))

(define scope : (-> Binding * Scope) stack)

(define (expression-binding-option ($expression : Expression)) : (Option Binding)
  (define $identifier-option (expression-generate-temporary-option $expression))
  (option-app binding
    $identifier-option
    (expression-syntax $expression)))

(define (tuple-binding-option-stack ($tuple : Tuple)) : (Stackof (Option Binding))
  (map expression-binding-option $tuple))

(define (binding-option-expression-bound-expression
  ($binding-option : (Option Binding))
  ($expression : Expression)) : Expression
  (expression
    (or
      (option-app binding-identifier $binding-option)
      (expression-syntax $expression))
    (expression-type $expression)))

(define (binding-option-stack-tuple-bound-tuple
  ($binding-option-stack : (Stackof (Option Binding)))
  ($tuple : Tuple)) : Tuple
  (map binding-option-expression-bound-expression $binding-option-stack $tuple))
