#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/compiler/type)

(define (type-symbol ($type : Type)) : Symbol
  (cond
    ((field? $type) (field-symbol $type))
    ((choice? $type) `choice)
    ((racket? $type) `racket)
    ((arrow? $type) `recipe)
    ((generic? $type) (type-symbol (generic-type $type)))
    ((recursive? $type) (type-symbol (recursive-type $type)))
    ((variable? $type) (error "impossible"))
    ((universe? $type) `universe)))

(check-equal? (type-symbol (null-field `foo)) `foo)
(check-equal? (type-symbol (choice null)) `choice)
(check-equal? (type-symbol (racket)) `racket)
(check-equal? (type-symbol (arrow null null)) `recipe)
(check-equal? (type-symbol (generic (null-field `foo))) `foo)
(check-equal? (type-symbol (recursive (null-field `foo))) `foo)
(check-equal? (type-symbol (universe 0)) `universe)
