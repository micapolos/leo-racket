#lang typed/racket/base

(require leo/compiler/leo)

(leo
  (point
    (x 10)
    (y 20))
  (do
    point x number
    (plus point y number))
  text 
  (plus "!!!"))
