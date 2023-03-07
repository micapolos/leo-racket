#lang typed/racket/base

(require leo/compiler/leo)

(leo
  10
  "foo"
  (do 
    number 
    (times number)
    text
    (plus "!!!")))