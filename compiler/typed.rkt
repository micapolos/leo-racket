#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/base)

(data (Value Type) typed (value : Value) (type : Type))
