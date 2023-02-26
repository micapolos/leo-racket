#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/racket
  leo/typed/type
  leo/typed/types
  leo/typed/testing)

(define (type-symbol ($type : Type)) : (Option Symbol)
  (cond
    ((racket? $type) 
      (define $any (racket-any $type))
      (if (symbol? $any) $any #f))
    ((tuple? $type) (tuple-symbol $type))
    ((arrow? $type) `giving)
    ((any? $type) `any)
    ((thing? $type) `thing)))

(check-equal? (type-symbol (racket `foo)) `foo)
(check-equal? (type-symbol (racket "non-symbol")) #f)
(check-equal? (type-symbol (tuple `foo null)) `foo)
(check-equal? (type-symbol (arrow null (thing))) `giving)
(check-equal? (type-symbol (any number-type)) `any)
(check-equal? (type-symbol (thing)) `thing)
