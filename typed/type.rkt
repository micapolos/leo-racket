#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/racket)

; TODO: Refactor to (struct type ((any : Any)))
(define-type Type (U Thing Racket Tuple Arrow TypeAny))

; type of everything
(data thing)

; function type
(data arrow (lhs-types : (Listof Type)) (rhs-type : Type))

; tuple type
(data tuple (symbol : Symbol) (type-list : (Listof Type)))

; choice type
(data choice (symbol : Symbol) (type-list : (Listof Type)))

; type of types
(struct any ((type : Type))
  #:transparent
  #:type-name TypeAny)

(define (type-arrow ($type : Type)) : Arrow
  (cond
    ((arrow? $type) $type)
    (else (error (format "not arrow ~v" $type)))))
