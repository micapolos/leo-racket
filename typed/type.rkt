#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/testing)

(define-type Type Any)

(struct native-type ((any : Any))
  #:type-name NativeType
  #:transparent)

(struct arrow-type ((lhs-types : (Listof Type)) (rhs-types : (Listof Type)))
  #:transparent
  #:type-name ArrowType)

(struct type-type ((type : Type))
  #:transparent
  #:type-name TypeType)

(struct thing-type ()
  #:transparent
  #:type-name ThingType)

(define (type-arrow $type) : ArrowType
  (cond
    ((arrow-type? $type) $type)
    (else (error (format "not arrow ~v" $type)))))
