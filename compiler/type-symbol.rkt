#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/compiler/type)

(define (type-symbol ($type : Type)) : Symbol
  (cond
    ((field? $type) (field-symbol $type))
    ((racket? $type) `racket)
    ((arrow? $type) `function)
    ((a? $type) `a)))

(check-equal? (type-symbol (field `foo null)) `foo)
(check-equal? (type-symbol (racket)) `racket)
(check-equal? (type-symbol (arrow null null)) `function)
(check-equal? (type-symbol (a (racket))) `a)
