#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/package)

(define-type Tail (U (Stackof Expression) Package))

(define null-tail : (Stackof Expression) null)
