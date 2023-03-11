#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack)

(define-type Type 
  (U 
    Racket 
    Field 
    Choice 
    Arrow 
    Generic
    Recursive 
    Variable
    Universe))

(define-type Structure (Stackof Type))

(data arrow 
  (lhs-structure : Structure) 
  (rhs-structure : Structure))

(data field 
  (symbol : Symbol) 
  (structure : Structure))

(data choice (structure : Structure))

(data racket)

(data generic (type : Type))

(data recursive (type : Type))

(data variable (index : Exact-Nonnegative-Integer))

(data universe (index : Exact-Nonnegative-Integer))

(define structure stack)

(define null-structure null)

(define racket-type (racket))

(define (racket-field ($symbol : Symbol)) 
  (field $symbol (structure (racket))))

(define (null-field ($symbol : Symbol)) 
  (field $symbol null-structure))

(define (type-universe ($type : Type)) : Universe
  (if (universe? $type) 
    (universe (add1 (universe-index $type)))
    (universe 0)))
