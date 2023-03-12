#lang racket/base

(provide (all-defined-out))

(define (fibonacci number)
  (if (< number 2) 
    number 
    (+
      (fibonacci (- number 2)) 
      (fibonacci (- number 1)))))

(time (fibonacci 42))

(define (leonardo number)
  (if (< number 2) 
    1
    (add1
      (+
        (leonardo (- number 2))
        (leonardo (- number 1))))))

(time (leonardo 42))
