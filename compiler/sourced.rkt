#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/base)

(data (Value) sourced (value : Value) (srcloc : (Option srcloc)))
