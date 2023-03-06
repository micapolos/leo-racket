#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/expressions)

(define-type Tail (U Tuple Expressions))

(define null-tail : Tuple null)
