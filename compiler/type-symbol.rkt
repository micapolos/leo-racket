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
    ((a? $type) `type)))

(check-equal? (type-symbol (field `foo null)) `foo)
(check-equal? (type-symbol (choice null)) `choice)
(check-equal? (type-symbol (racket)) `racket)
(check-equal? (type-symbol (arrow null null)) `recipe)
(check-equal? (type-symbol (a (racket))) `type)
