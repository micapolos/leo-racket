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

(define-type Tuple (Stackof Expression))

(define tuple stack)

(define null-tuple null)
