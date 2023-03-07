#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack)

(define-type Type (U Racket Field Arrow A))

(define-type Structure (Stackof Type))

(data arrow 
  (lhs-structure : Structure) 
  (rhs-structure : Structure))

(data field 
  (symbol : Symbol) 
  (structure : Structure))

(data racket)

(data a (type : Type))

(define structure stack)

(define null-structure null)

(define (racket-field ($symbol : Symbol)) 
  (field $symbol (structure (racket))))

(define (null-field ($symbol : Symbol)) 
  (field $symbol null-structure))
