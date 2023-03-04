#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/package)

(define-type Tail (U Tuple Package))

(define null-tail : Tuple null)
