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
    ((native-type? $type) 
      (define $any (native-type-any $type))
      (if (symbol? $any) $any #f))
    ((list? $type) (let-in $car (car $type) (and (symbol? $car) $car)))
    ((arrow-type? $type) `giving)
    ((type-type? $type) `any)
    ((thing-type? $type) `thing)
    (else #f)))

(check-equal? (type-symbol (native-type `foo)) `foo)
(check-equal? (type-symbol (native-type "non-symbol")) #f)
(check-equal? (type-symbol `foo) `foo)
(check-equal? (type-symbol `(foo)) `foo)
(check-equal? (type-symbol (arrow-type null null)) `giving)
(check-equal? (type-symbol (type-type number-type)) `any)
(check-equal? (type-symbol (thing-type)) `thing)
