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

; TODO: Refactor expression-syntax
(define (expression-syntax-option ($expression : Expression)) : (Option Syntax)
  (and 
    (type-dynamic? (expression-type $expression)) 
    (expression-syntax $expression)))

(define-type Tuple (Stackof Expression))

(define tuple stack)

(define null-tuple null)

(define (empty-expression ($type : Type)) : Expression 
  (expression null-syntax $type))
