#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/compiler/racket
  leo/compiler/type)

(define (type-symbol ($type : Type)) : Symbol
  (cond
    ((field? $type) (field-symbol $type))
    ((racket? $type) 
      (define $any (racket-any $type))
      (cond
        ((symbol? $any) $any)
        (else `any)))
    ((arrow? $type) `function)
    ((a? $type) `type)))

(check-equal? (type-symbol (field `foo null)) `foo)
(check-equal? (type-symbol (racket `foo)) `foo)
(check-equal? (type-symbol (racket 123)) `any)
(check-equal? (type-symbol (arrow null null)) `function)
(check-equal? (type-symbol (a (structure (racket `foo)))) `type)
