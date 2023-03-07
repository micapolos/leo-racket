#lang typed/racket/base

(require leo/compiler/leo)

(leo
  (point
      (int 1)
      (plus (int 2))))
