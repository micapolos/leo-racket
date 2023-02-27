#lang typed/racket/base

(provide (all-defined-out))

(struct (Value) indexed ((index : Exact-Nonnegative-Integer) (value : Value))
  #:transparent
  #:type-name Indexed)
