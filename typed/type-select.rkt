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
    ((equal? $selector (symbol-type `boolean)) 
      (equal? $type boolean-type))
    ((equal? $selector (symbol-type `number)) 
      (equal? $type number-type))
    ((equal? $selector (symbol-type `fixnum)) 
      (equal? $type fixnum-type))
    ((equal? $selector (symbol-type `flonum)) 
      (equal? $type flonum-type))
    ((equal? $selector (symbol-type `string)) 
      (equal? $type string-type))
    ((equal? $selector (symbol-type `function)) 
      (arrow-type? $type))
    ((equal? $selector (symbol-type `any)) 
      (type-type? $type))
    ((and (field-type? $selector) (equal? (field-type-symbol $selector) `field))
      (define $selector-body (field-type-body $selector))
      (if 
        (and 
          (struct-type-body? $selector-body)
          (= (length (struct-type-body-type-list $selector-body)) 1))
        (type-selects-field?
          $type 
          (car (struct-type-body-type-list $selector-body)))
        #f))
    (else 
      (type-selects-field? $type $selector))))

(define (type-selects-field? ($type : Type) ($selector : Type)) : Boolean
  (and 
    (symbol-type? $selector)
    (field-type? $type) 
    (equal? (field-type-symbol $type) (symbol-type-symbol $selector))))

(check-equal? (type-selects? (symbol-type `foo) (symbol-type `foo)) #t)
(check-equal? (type-selects? (symbol-type `foo) (symbol-type `not-foo)) #f)

(check-equal? (type-selects? boolean-type (symbol-type `boolean)) #t)
(check-equal? (type-selects? boolean-type (symbol-type `not-boolean)) #f)

(check-equal? (type-selects? number-type (symbol-type `number)) #t)
(check-equal? (type-selects? number-type (symbol-type `not-number)) #f)

(check-equal? (type-selects? string-type (symbol-type `string)) #t)
(check-equal? (type-selects? string-type (symbol-type `not-string)) #f)

(check-equal? 
  (type-selects?
    (arrow-type (list number-type) (list string-type))
    (symbol-type `function))
  #t)
(check-equal? 
  (type-selects?
    (arrow-type (list number-type) (list string-type))
    (symbol-type `not-function))
  #f)

(check-equal? 
  (type-selects? 
    (field-type `foo void-type-body)
    (symbol-type `foo)) #t)
(check-equal? 
  (type-selects? 
    (field-type `foo void-type-body)
    (symbol-type `not-foo)) #f)

(check-equal? 
  (type-selects? 
    (type-type number-type)
    (symbol-type `any)) #t)

(check-equal? 
  (type-selects? 
    (type-type number-type)
    (symbol-type `not-any)) #f)

(check-equal? 
  (type-selects? 
    (field-type `number void-type-body)
    (field-type `field (struct-type-body (list (symbol-type `number))))) #t)
(check-equal? 
  (type-selects? 
    (field-type `number void-type-body)
    (field-type `field (struct-type-body (list (symbol-type `not-number))))) #f)

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
    ((and (equal? $selector (symbol-type `first)) (>= (length $type-list) 1))
      (cons 
        0 
        (field-type `first (struct-type-body (list (list-ref $type-list 0))))))
    ((and (equal? $selector (symbol-type `second)) (>= (length $type-list) 2))
      (cons 
        1 
        (field-type `second (struct-type-body (list (list-ref $type-list 1))))))
    (else (type-list-select-from $type-list $selector 0))))

(define (struct-type-body-select
    ($struct-type-body : StructTypeBody)
    ($selector : Type))
  (type-list-select 
    (struct-type-body-type-list $struct-type-body) 
    $selector))

(define (type-body-select
    ($type-body : TypeBody)
    ($selector : Type))
  (cond
    ((struct-type-body? $type-body) 
      (struct-type-body-select $type-body $selector))
    ((choice-type-body? $type-body) #f)))

(check-equal?
  (type-list-select
    (list number-type (symbol-type `foo) string-type)
    (symbol-type `number))
  (cons 0 number-type))

(check-equal?
  (type-list-select
    (list number-type (symbol-type `foo) string-type)
    (symbol-type `foo))
  (cons #f (symbol-type `foo)))

(check-equal?
  (type-list-select
    (list number-type (symbol-type `foo) string-type)
    (symbol-type `string))
  (cons 1 string-type))

(check-equal?
  (type-list-select
    (list number-type (symbol-type `foo) string-type)
    (symbol-type `boolean))
  #f)

(check-equal?
  (type-list-select
    (list number-type)
    (symbol-type `number))
  (cons 0 number-type))

(check-equal?
  (type-list-select
    (list number-type (symbol-type `foo))
    (symbol-type `number))
  (cons 0 number-type))

(check-equal?
  (type-list-select
    (list number-type string-type)
    (symbol-type `first))
  (cons 0 (field-type `first (struct-type-body (list number-type)))))

(check-equal?
  (type-list-select
    (list number-type string-type)
    (symbol-type `second))
  (cons 1 (field-type `second (struct-type-body (list string-type)))))
