#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/testing
  leo/typed/type
  leo/typed/types)

; ---------------------------------------------------------

(define (type-is-dynamic? ($type : Type)) : Boolean
  (not (type-is-static? $type)))

(define (type-is-static? ($type : Type)) : Boolean
  (cond
    ((symbol? $type) #t)
    ((list? $type) (andmap type-is-static? $type))
    ((racket? $type) #f)
    ((giving? $type) 
      (andmap type-is-static? (giving-rhs-types $type)))
    ((any? $type) #t)
    ((thing? $type) #f)
    (else #t)))

(let ()
  (define static-type `(foo))
  (define non-static-type number-type)

  (check-equal? (type-is-static? (racket `foo)) #f)
  (check-equal? (type-is-static? boolean-type) #f)
  (check-equal? (type-is-static? string-type) #f)
  (check-equal? (type-is-static? number-type) #f)

  (check-equal? (type-is-static? `(foo)) #t)
  (check-equal? (type-is-static? `(foo ,number-type)) #f)

  (check-equal? (type-is-static? (giving (list static-type) (list static-type))) #t)
  (check-equal? (type-is-static? (giving (list non-static-type) (list static-type))) #t)
  (check-equal? (type-is-static? (giving (list static-type) (list non-static-type))) #f)
  (check-equal? (type-is-static? (giving (list non-static-type) (list non-static-type))) #f)

  (check-equal? (type-is-static? (any number-type)) #t)

  (check-equal? (type-is-static? (thing)) #f))

; ---------------------------------------------------------

(define (type-list-size ($type-list : (Listof Type))) : Index
  (length (filter type-is-dynamic? $type-list)))

(check-equal?
  (type-list-size (list number-type `foo string-type))
  2)

; ---------------------------------------------------------

(define (field-type? ($type : Type)) : Boolean
  (and
    (not (null? $type))
    (list? $type)
    (symbol? (car $type))))

(check-equal? (field-type? `foo) #f)
(check-equal? (field-type? `()) #f)
(check-equal? (field-type? `(foo)) #t)
(check-equal? (field-type? `(foo 1)) #t)
(check-equal? (field-type? `(1 2)) #f)
