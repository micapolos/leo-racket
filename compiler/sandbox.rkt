#lang typed/racket/base

(require leo/compiler/leo)

(leo
  (vector
    10
    (point
      20
      (plus 1)
      (times 12)
      30)))
