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
    ((generic? $type) `generic) ; TODO: Get from $type-stack
    ((specific? $type) (type-symbol (specific-body-type $type))) ; TODO: Get from $type-stack
    ((recursive? $type) (type-symbol (recursive-type $type))) ; TODO: Get from $type-stack
    ((variable? $type) (error "impossible")) ; TODO: Get from $type-stack
    ((universe? $type) `universe)
    ((value? $type) `value)))

(check-equal? (type-symbol (field! `foo)) `foo)
(check-equal? (type-symbol (choice null)) `choice)
(check-equal? (type-symbol (racket)) `racket)
(check-equal? (type-symbol (arrow null null)) `recipe)
(check-equal? (type-symbol (generic (field! `foo))) `generic)
(check-equal? (type-symbol (specific (field! `foo) (field! `bar))) `bar)
(check-equal? (type-symbol (recursive (field! `foo))) `foo)
(check-equal? (type-symbol (universe 0)) `universe)
