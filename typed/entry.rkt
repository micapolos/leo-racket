#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base)

(data (K V) entry
  (key : K)
  (value : V))
