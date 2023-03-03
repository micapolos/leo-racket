#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/expressions)

(define-type Tail (U (Stackof Expression) Expressions))

(define null-tail : (Stackof Expression) null)
