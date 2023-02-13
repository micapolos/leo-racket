#lang typed/racket

(provide (all-defined-out))

(require leo/typed/type)

(struct (V) typed (($value : V) ($type : Type))
  #:transparent
  #:type-name Typed)
