#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/compiler/racket)

(define-type Type (U Racket Field Arrow A))

(define-type Structure (Stackof Type))

(data arrow (lhs-structure : Structure) (rhs-structure : Structure))

(data field (symbol : Symbol) (structure : Structure))

(data a (type : Type))
