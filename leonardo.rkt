#lang racket/base

(provide (all-defined-out))

(define (leonardo number)
  (if (< number 2) 
    1
    (add1
      (+
        (leonardo (- number 2))
        (leonardo (- number 1))))))

(time (leonardo 42))
