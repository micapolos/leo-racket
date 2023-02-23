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
    ((native-type? $type) #f)
    ((symbol-type? $type) #t)
    ((field-type? $type) (type-body-is-static? (field-type-body $type)))
    ((arrow-type? $type) 
      (andmap type-is-static? (arrow-type-rhs-types $type)))
    ((type-type? $type) #t)
    ((thing-type? $type) #f)))

(define (type-body-is-static? ($type-body : TypeBody)) : Boolean
  (cond
    ((struct-type-body? $type-body) 
      (andmap type-is-static? 
        (struct-type-body-type-list $type-body)))
    ((choice-type-body? $type-body) #f)))

(let ()
  (define static-type (field-type `foo void-type-body))
  (define non-static-type number-type)

  (define static-type-body void-type-body)
  (define non-static-type-body (struct-type-body (list non-static-type)))

  (check-equal? (type-is-static? (native-type `foo)) #f)
  (check-equal? (type-is-static? boolean-type) #f)
  (check-equal? (type-is-static? string-type) #f)
  (check-equal? (type-is-static? number-type) #f)

  (check-equal? (type-body-is-static? (struct-type-body null)) #t)
  (check-equal? (type-body-is-static? (struct-type-body (list number-type))) #f)

  (check-equal? (type-is-static? (field-type `foo static-type-body)) #t)
  (check-equal? (type-is-static? (field-type `foo non-static-type-body)) #f)

  (check-equal? (type-is-static? (arrow-type (list static-type) (list static-type))) #t)
  (check-equal? (type-is-static? (arrow-type (list non-static-type) (list static-type))) #t)
  (check-equal? (type-is-static? (arrow-type (list static-type) (list non-static-type))) #f)
  (check-equal? (type-is-static? (arrow-type (list non-static-type) (list non-static-type))) #f)

  (check-equal? (type-is-static? (type-type number-type)) #t)

  (check-equal? (type-is-static? (thing-type)) #f)

  (check-equal? (type-body-is-static? (struct-type-body (list))) #t)
  (check-equal? (type-body-is-static? (struct-type-body (list static-type))) #t)
  (check-equal? (type-body-is-static? (struct-type-body (list static-type static-type))) #t)
  (check-equal? (type-body-is-static? (struct-type-body (list non-static-type))) #f)
  (check-equal? (type-body-is-static? (struct-type-body (list static-type non-static-type))) #f)
  
  (check-equal? (type-body-is-static? (choice-type-body null)) #f)
  (check-equal? (type-body-is-static? (choice-type-body (list static-type))) #f))

; ---------------------------------------------------------

(define (struct-type-body-size ($struct-type-body : StructTypeBody)) : Exact-Nonnegative-Integer
  (length (filter type-is-dynamic? (struct-type-body-type-list $struct-type-body))))

(check-equal?
  (struct-type-body-size 
    (struct-type-body 
      (list number-type (symbol-type `foo) string-type)))
  2)
