#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/racket
  leo/typed/stack)

(define-type Type (U Racket Field Arrow))

(struct arrow ((lhs-type-stack : (Stackof Type)) (rhs-type : Type))
  #:transparent
  #:type-name Arrow)

(struct field ((symbol : Symbol) (type-stack : (Stackof Type)))
  #:transparent
  #:type-name Field)
