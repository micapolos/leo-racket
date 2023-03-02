#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/types
  leo/typed/option
  leo/typed/type-utils
  leo/typed/testing)

; ------------------------------------------------------------------

(define (type-selects? ($type : Type) ($selector : Type)) : Boolean
  (cond
    ((equal? $type $selector) #t)
    ((equal? $selector (tuple `boolean null))
      (equal? $type boolean-type))
    ((equal? $selector (tuple `thing null))
      (equal? $type (thing)))
    ((equal? $selector (tuple `number null))
      (equal? $type number-type))
    ((equal? $selector (tuple `fixnum null))
      (equal? $type fixnum-type))
    ((equal? $selector (tuple `flonum null))
      (equal? $type flonum-type))
    ((equal? $selector (tuple `string null))
      (equal? $type string-type))
    ((equal? $selector (tuple `giving null))
      (arrow? $type))
    ((equal? $selector (tuple `any null))
      (any? $type))
    (else 
      (type-selects-field? $type $selector))))

(define (type-selects-field? ($type : Type) ($selector : Type)) : Boolean
  (and 
    (tuple? $type)
    (tuple? $selector)
    (null? (tuple-type-list $selector))
    (equal? (tuple-symbol $type) (tuple-symbol $selector))))

(check-equal? (type-selects? (tuple `foo null) (tuple `foo null)) #t)
(check-equal? (type-selects? (tuple `foo null) (tuple `not-foo null)) #f)

(check-equal? (type-selects? boolean-type (tuple `boolean null)) #t)
(check-equal? (type-selects? boolean-type (tuple `not-boolean null)) #f)

(check-equal? (type-selects? number-type (tuple `number null)) #t)
(check-equal? (type-selects? number-type (tuple `not-number null)) #f)

(check-equal? (type-selects? string-type (tuple `string null)) #t)
(check-equal? (type-selects? string-type (tuple `not-string null)) #f)

(check-equal? (type-selects? (thing) (tuple `thing null)) #t)
(check-equal? (type-selects? (thing) (tuple `not-thing null)) #f)

(check-equal? 
  (type-selects?
    (arrow (list number-type) string-type)
    (tuple `giving null))
  #t)

(check-equal? 
  (type-selects?
    (arrow (list number-type) string-type)
    (tuple `not-giving null))
  #f)

(check-equal? 
  (type-selects? 
    (tuple `foo null)
    (tuple `foo null)) #t)

(check-equal? 
  (type-selects? 
    (tuple `foo null)
    (tuple `not-foo null)) 
  #f)

(check-equal? 
  (type-selects? 
    (any number-type)
    (tuple `any null))
  #t)

(check-equal? 
  (type-selects? 
    (any number-type)
    (tuple `not-any null))
  #f)

; ------------------------------------------------------------------

(define
  (type-list-select-from
    ($type-list : (Listof Type))
    ($selector : Type)
    ($index : Exact-Nonnegative-Integer)) : 
      (Option (Pairof (Option Exact-Nonnegative-Integer) Type))
  (cond
    ((null? $type-list) #f)
    ((type-selects? (car $type-list) $selector) 
      (cons 
        (if (type-static? (car $type-list)) #f $index)
        (car $type-list)))
    (else 
      (type-list-select-from
        (cdr $type-list)
        $selector
        (if (type-static? (car $type-list))
          $index
          (+ $index 1))))))

(define 
  (type-list-select
    ($type-list : (Listof Type))
    ($selector : Type)) : (Option (Pairof (Option Exact-Nonnegative-Integer) Type))
  (cond
    ((and (equal? $selector (tuple `first null)) (>= (length $type-list) 1))
      (cons 
        0 
        (tuple `first (list (list-ref $type-list 0)))))
    ((and (equal? $selector (tuple `second null)) (>= (length $type-list) 2))
      (cons 
        1 
        (tuple `second (list (list-ref $type-list 1)))))
    (else (type-list-select-from $type-list $selector 0))))

(check-equal?
  (type-list-select
    (list number-type (tuple `foo null) string-type)
    (tuple `number null))
  (cons 0 number-type))

(check-equal?
  (type-list-select
    (list number-type (tuple `foo null) string-type)
    (tuple `foo null))
  (cons #f (tuple `foo null)))

(check-equal?
  (type-list-select
    (list number-type (tuple `foo null) string-type)
    (tuple `string null))
  (cons 1 string-type))

(check-equal?
  (type-list-select
    (list number-type (tuple `foo null) string-type)
    (tuple `boolean null))
  #f)

(check-equal?
  (type-list-select
    (list number-type)
    (tuple `number null))
  (cons 0 number-type))

(check-equal?
  (type-list-select
    (list number-type (tuple `foo null))
    (tuple `number null))
  (cons 0 number-type))

(check-equal?
  (type-list-select
    (list number-type string-type)
    (tuple `first null))
  (cons 0 (tuple `first (list number-type))))

(check-equal?
  (type-list-select
    (list number-type string-type)
    (tuple `second null))
  (cons 1 (tuple `second (list string-type))))
