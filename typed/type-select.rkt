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
    ((equal? $selector `boolean)
      (equal? $type boolean-type))
    ((equal? $selector `thing)
      (equal? $type (thing)))
    ((equal? $selector `number)
      (equal? $type number-type))
    ((equal? $selector `fixnum)
      (equal? $type fixnum-type))
    ((equal? $selector `flonum)
      (equal? $type flonum-type))
    ((equal? $selector `string)
      (equal? $type string-type))
    ((equal? $selector `giving)
      (arrow? $type))
    ((equal? $selector `any)
      (any? $type))
    (else 
      (type-selects-field? $type $selector))))

(define (type-selects-field? ($type : Type) ($selector : Type)) : Boolean
  (and 
    (symbol? $selector)
    (not (null? $type))
    (list? $type)
    (equal? (car $type) $selector)))

(check-equal? (type-selects? `foo `foo) #t)
(check-equal? (type-selects? `foo `not-foo) #f)

(check-equal? (type-selects? boolean-type `boolean) #t)
(check-equal? (type-selects? boolean-type `not-boolean) #f)

(check-equal? (type-selects? number-type `number) #t)
(check-equal? (type-selects? number-type `not-number) #f)

(check-equal? (type-selects? string-type `string) #t)
(check-equal? (type-selects? string-type `not-string) #f)

(check-equal? (type-selects? (thing) `thing) #t)
(check-equal? (type-selects? (thing) `not-thing) #f)

(check-equal? 
  (type-selects?
    (arrow (list number-type) (list string-type))
    `giving)
  #t)
(check-equal? 
  (type-selects?
    (arrow (list number-type) (list string-type))
    `not-giving)
  #f)

(check-equal? 
  (type-selects? 
    `(foo)
    `foo) #t)
(check-equal? 
  (type-selects? 
    `(foo)
    `not-foo) #f)

(check-equal? 
  (type-selects? 
    (any number-type)
    `any) #t)

(check-equal? 
  (type-selects? 
    (any number-type)
    `not-any) #f)

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
        (if (type-is-static? (car $type-list)) #f $index)
        (car $type-list)))
    (else 
      (type-list-select-from
        (cdr $type-list)
        $selector
        (if (type-is-static? (car $type-list))
          $index
          (+ $index 1))))))

(define 
  (type-list-select
    ($type-list : (Listof Type))
    ($selector : Type)) : (Option (Pairof (Option Exact-Nonnegative-Integer) Type))
  (cond
    ((and (equal? $selector `first) (>= (length $type-list) 1))
      (cons 
        0 
        `(first ,(list-ref $type-list 0))))
    ((and (equal? $selector `second) (>= (length $type-list) 2))
      (cons 
        1 
        `(second ,(list-ref $type-list 1))))
    (else (type-list-select-from $type-list $selector 0))))

(check-equal?
  (type-list-select
    (list number-type `foo string-type)
    `number)
  (cons 0 number-type))

(check-equal?
  (type-list-select
    (list number-type `foo string-type)
    `foo)
  (cons #f `foo))

(check-equal?
  (type-list-select
    (list number-type `foo string-type)
    `string)
  (cons 1 string-type))

(check-equal?
  (type-list-select
    (list number-type `foo string-type)
    `boolean)
  #f)

(check-equal?
  (type-list-select
    (list number-type)
    `number)
  (cons 0 number-type))

(check-equal?
  (type-list-select
    (list number-type `foo)
    `number)
  (cons 0 number-type))

(check-equal?
  (type-list-select
    (list number-type string-type)
    `first)
  (cons 0 `(first ,number-type)))

(check-equal?
  (type-list-select
    (list number-type string-type)
    `second)
  (cons 1 `(second ,string-type)))
