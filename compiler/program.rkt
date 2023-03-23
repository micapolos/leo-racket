#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/compiler/ingredients
  leo/compiler/ingredients-utils)

(data program
  (used-ingredients : Ingredients)
  (done-ingredients : Ingredients))

(define null-program
  (program null-ingredients null-ingredients))

(define (program-plus-used-ingredients
  ($program : Program)
  ($used-ingredients : Ingredients))
: Program
  (unless (null? (program-done-ingredients $program))
    (error "can not use after started doing"))
  (struct-copy program $program
    (used-ingredients
      (ingredients-plus
        (program-used-ingredients $program)
        $used-ingredients))))

(define (program-with-done-ingredients
  ($program : Program)
  ($done-ingredients : Ingredients))
: Program
  (struct-copy program $program
    (done-ingredients $done-ingredients)))
