#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing)

(data position
  (line-number : Exact-Positive-Integer)
  (char-number : Exact-Positive-Integer))

(define start-position (position 1 1))

(define (position-newline ($position : Position)) : Position
  (position
    (add1 (position-line-number $position))
    1))

(define (position-next-char ($position : Position)) : Position
  (position
    (position-line-number $position)
    (add1 (position-char-number $position))))

(define (position-plus-char ($position : Position) ($char : Char)) : Position
  (case $char
    ((#\newline) (position-newline $position))
    (else (position-next-char $position))))

(check-equal? (position-newline (position 3 8)) (position 4 1))
(check-equal? (position-next-char (position 3 8)) (position 3 9))
(check-equal? (position-plus-char (position 3 8) #\newline) (position-newline (position 3 8)))
(check-equal? (position-plus-char (position 3 8) #\a) (position-next-char (position 3 8)))
