#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/type
  leo/typed/types
  leo/typed/testing)

(define (type-symbol ($type : Type)) : (Option Symbol)
  (cond
    ((null? $type) #f)
    ((symbol? $type) $type)
    ((racket? $type) 
      (define $any (racket-any $type))
      (if (symbol? $any) $any #f))
    ((list? $type) (let-in $car (car $type) (and (symbol? $car) $car)))
    ((arrow? $type) `giving)
    ((any? $type) `any)
    ((thing? $type) `thing)
    (else #f)))

(check-equal? (type-symbol (racket `foo)) `foo)
(check-equal? (type-symbol (racket "non-symbol")) #f)
(check-equal? (type-symbol `foo) `foo)
(check-equal? (type-symbol `(foo)) `foo)
(check-equal? (type-symbol (arrow null null)) `giving)
(check-equal? (type-symbol (any number-type)) `any)
(check-equal? (type-symbol (thing)) `thing)
