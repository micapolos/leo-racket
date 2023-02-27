#lang typed/racket/base

(provide (all-defined-out))

(struct (Value) sourced ((value : Value) (srcloc : srcloc))
  #:transparent
  #:type-name Sourced)
