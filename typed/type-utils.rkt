#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/racket
  leo/typed/testing
  leo/typed/type
  leo/typed/types)

; ---------------------------------------------------------

(define (type-is-dynamic? ($type : Type)) : Boolean
  (not (type-is-static? $type)))

(define (type-is-static? ($type : Type)) : Boolean
  (cond
    ((thing? $type) #f)
    ((racket? $type) #f)
    ((tuple? $type) (andmap type-is-static? (tuple-type-list $type)))
    ((arrow? $type) #f)
    ((any? $type) #t)))

(define static-type (tuple `foo null))
(define non-static-type number-type)

(let ()
  (check-equal? (type-is-static? (racket `foo)) #f)
  (check-equal? (type-is-static? boolean-type) #f)
  (check-equal? (type-is-static? string-type) #f)
  (check-equal? (type-is-static? number-type) #f)

  (check-equal? (type-is-static? (tuple `foo null)) #t)
  (check-equal? (type-is-static? (tuple `foo (list number-type))) #f)

  (check-equal? (type-is-static? (arrow (list static-type) static-type)) #f)
  (check-equal? (type-is-static? (arrow (list non-static-type) static-type)) #f)
  (check-equal? (type-is-static? (arrow (list static-type) non-static-type)) #f)
  (check-equal? (type-is-static? (arrow (list non-static-type) non-static-type)) #f)

  (check-equal? (type-is-static? (any number-type)) #t)

  (check-equal? (type-is-static? (thing)) #f))

; ---------------------------------------------------------

(define (type-list-size ($type-list : (Listof Type))) : Index
  (length (filter type-is-dynamic? $type-list)))

(check-equal?
  (type-list-size (list number-type static-type string-type))
  2)
