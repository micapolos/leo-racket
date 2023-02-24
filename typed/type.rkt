#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/testing)

; TODO: Refactor to (struct type ((any : Any)))
(define-type Type Any)

(struct racket ((any : Any))
  #:type-name Racket
  #:transparent)

(struct giving ((lhs-types : (Listof Type)) (rhs-types : (Listof Type)))
  #:transparent
  #:type-name Arrow)

(struct any ((type : Type))
  #:transparent
  #:type-name TypeAny)

(struct thing ()
  #:transparent
  #:type-name Thing)

(define (type-arrow ($type : Type)) : Arrow
  (cond
    ((giving? $type) $type)
    (else (error (format "not arrow ~v" $type)))))
