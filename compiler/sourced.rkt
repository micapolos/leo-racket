#lang typed/racket/base

(provide (all-defined-out))

(struct (Value) sourced ((value : Value) (srcloc : (Option srcloc)))
  #:transparent
  #:type-name Sourced)
