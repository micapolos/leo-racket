#lang typed/racket/base

(require leo/compiler/leo)

(leo
  1
  (plus 3)
  text
  (plus " ")
  (plus "apples")
  (do
    text
    (plus ", ")
    (plus text)
    (plus "!!!")))
