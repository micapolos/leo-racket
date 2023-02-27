#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/type
  leo/compiler/racket
  leo/typed/stack
  leo/typed/testing)

(define (type-is-dynamic? ($type : Type)) : Boolean
  (cond
    ((racket? $type) #t)
    ((arrow? $type) #t)
    ((field? $type) (ormap type-is-dynamic? (field-type-stack $type)))))

(check-equal? (type-is-dynamic? (racket `number)) #t)
(check-equal? (type-is-dynamic? (arrow null null)) #t)
(check-equal? (type-is-dynamic? (field `foo null)) #f)
(check-equal? (type-is-dynamic? (field `foo (list (field `foo null)))) #f)
(check-equal? (type-is-dynamic? (field `foo (list (racket `number)))) #t)
(check-equal? (type-is-dynamic? (field `foo (list (field `foo null) (racket `number)))) #t)

(define (type-stack-size ($type-stack : (Stackof Type))) : Exact-Nonnegative-Integer
  (length (filter type-is-dynamic? $type-stack)))

