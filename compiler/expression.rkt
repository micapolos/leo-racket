#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-sexp)

(data expression (syntax : Syntax) (type : Type))

(define-type Selection (U Type Expression))

; TODO: Refactor expression-syntax
(define (expression-syntax-option ($expression : Expression)) : (Option Syntax)
  (and 
    (type-dynamic? (expression-type $expression)) 
    (expression-syntax $expression)))

(define-type Tuple (Stackof Expression))

(define tuple : (-> Expression * Tuple) stack)

(define null-tuple null)

(define (empty-expression ($type : Type)) : Expression 
  (expression null-syntax $type))

(define (expression-identifier-option ($expression : Expression)) : (Option Identifier)
  (define $syntax (expression-syntax $expression))
  (and (identifier? $syntax) $syntax))
