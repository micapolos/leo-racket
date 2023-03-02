#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/racket
  leo/typed/stack)

(define-type Type (U Racket Field Arrow A))

(data arrow 
  (lhs-type-stack : (Stackof Type)) 
  (rhs-type : Type))

(data field 
  (symbol : Symbol) 
  (type-stack : (Stackof Type)))

(data a (type : Type))
