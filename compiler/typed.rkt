#lang typed/racket/base

(provide (all-defined-out))

(struct (Value Type) typed ((value : Value) (type : Type))
  #:transparent
  #:type-name Typed)
