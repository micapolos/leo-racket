#lang typed/racket/base

(require leo/compiler/leo)

(leo
  1
  (plus 2)
  text
  (plus " apples")
  type)
