#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/entry)

(data (K V) dictionary
  (entry-stack : (Stackof (Entry K V))))
