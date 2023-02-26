#lang racket/base

(provide (all-defined-out))

(define (fibonacci number)
  (if (< number 2) 
    number 
    (+
      (fibonacci (- number 2)) 
      (fibonacci (- number 1)))))

;(time (fibonacci 42))
