#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/base)

; TODO: Refactor to (struct type ((any : Any)))
(define-type Type Any)

; type of everything
(data thing)

; native racket type
(data racket (any : Any))

; function type
(data arrow (lhs-types : (Listof Type)) (rhs-type : Type))

; field type
(data field (symbol : Symbol) (type-list : (Listof Type)))

; type of types
(struct any ((type : Type))
  #:transparent
  #:type-name TypeAny)

(define (type-arrow ($type : Type)) : Arrow
  (cond
    ((arrow? $type) $type)
    (else (error (format "not arrow ~v" $type)))))
