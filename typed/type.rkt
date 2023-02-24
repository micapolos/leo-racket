#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/testing)

; TODO: Refactor to (struct type ((any : Any)))
(define-type Type Any)

; unit type
(struct unit () #:transparent #:type-name Unit)

; primitive types (T suffix to avoid conflict with Typed Racket types)
(struct boolean () #:transparent #:type-name BooleanT)
(struct fixnum () #:transparent #:type-name FixnumT)
(struct flonum () #:transparent #:type-name FlonumT)
(struct number () #:transparent #:type-name NumberT)
(struct string () #:transparent #:type-name StringT)

; type of everything
(struct thing ()
  #:transparent
  #:type-name Thing)

; native racket type
(struct racket ((any : Any))
  #:type-name Racket
  #:transparent)

; function type
(struct giving ((lhs-types : (Listof Type)) (rhs-types : (Listof Type)))
  #:transparent
  #:type-name Arrow)

; type of types
(struct any ((type : Type))
  #:transparent
  #:type-name TypeAny)

(define (type-arrow ($type : Type)) : Arrow
  (cond
    ((giving? $type) $type)
    (else (error (format "not arrow ~v" $type)))))
