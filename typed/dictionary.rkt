#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack)

(data (K V) dictionary-entry
  (key : K)
  (value : V))

(data (K V) dictionary
  (entry-stack : (Stackof (Dictionary-Entry K V))))
