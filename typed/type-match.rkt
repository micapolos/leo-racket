#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type 
  leo/typed/testing)

(define (type-matches? (actual : Type) (expected : Type)) : Boolean
  (equal? actual expected))

(define (type-match (actual : Type) (expected : Type))
  (if (type-matches? actual expected) 
    (void)
    (error (format "Type mismatch, actual: ~a, expected: ~a" actual expected))))

(define (type-matching (actual : Type) (expected : Type)) : Type
  (if (type-matches? actual expected) 
    actual
    (error (format "Type mismatch, actual: ~a, expected: ~a" actual expected))))
