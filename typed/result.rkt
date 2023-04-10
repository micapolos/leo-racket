#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/failure)

(data (V) success (value : V))

(data (V E) result
  (value : (U (Success V) (Failure E))))
