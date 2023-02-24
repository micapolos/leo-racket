#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/testing)

; TODO: Refactor to (struct type ((any : Any)))
(define-type Type Any)

; everything matches this type
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
