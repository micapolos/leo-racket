#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/types
  leo/typed/type-is-static
  leo/testing)

(define 
  (type-list-selector-index
    ($type-list : (Listof Type))
    ($selector : Type)) : (Option Integer)
  (type-list-selector-index-from $type-list $selector 0))

(define 
  (type-list-selector-index-from
    ($type-list : (Listof Type))
    ($selector : Type)
    ($index : Integer)) : (Option Integer)
  (cond
    ((null? $type-list) #f)
    ((type-selector-selects? (car $type-list) $selector) $index)
    (else 
      (type-list-selector-index-from
        (cdr $type-list)
        $selector
        (if (type-is-static? (car $type-list))
          $index
          (+ $index 1))))))

(define (type-selector-selects? ($type : Type) ($selector : Type)) : Boolean
  (cond
    ((equal? $type $selector) #t)
    ((equal? $selector (symbol-type `boolean)) 
      (equal? $type boolean-type))
    ((equal? $selector (symbol-type `number)) 
      (equal? $type number-type))
    ((equal? $selector (symbol-type `string)) 
      (equal? $type string-type))
    ((equal? $selector (symbol-type `function)) 
      (arrow-type? $type))
    ((and (field-type? $selector) (equal? (field-type-symbol $selector) `field))
      (define $selector-body (field-type-body $selector))
      (if 
        (and 
          (struct-type-body? $selector-body)
          (= (length (struct-type-body-type-list $selector-body)) 1))
        (type-selector-selects-field?
          $type 
          (car (struct-type-body-type-list $selector-body)))
        #f))
    (else 
      (type-selector-selects-field? $type $selector))))

(define (type-selector-selects-field? ($type : Type) ($selector : Type)) : Boolean
  (and 
    (symbol-type? $selector)
    (field-type? $type) 
    (equal? (field-type-symbol $type) (symbol-type-symbol $selector))))

(check-equal? (type-selector-selects? (symbol-type `foo) (symbol-type `foo)) #t)
(check-equal? (type-selector-selects? (symbol-type `foo) (symbol-type `not-foo)) #f)

(check-equal? (type-selector-selects? boolean-type (symbol-type `boolean)) #t)
(check-equal? (type-selector-selects? boolean-type (symbol-type `not-boolean)) #f)

(check-equal? (type-selector-selects? number-type (symbol-type `number)) #t)
(check-equal? (type-selector-selects? number-type (symbol-type `not-number)) #f)

(check-equal? (type-selector-selects? string-type (symbol-type `string)) #t)
(check-equal? (type-selector-selects? string-type (symbol-type `not-string)) #f)

(check-equal? 
  (type-selector-selects?
    (arrow-type (list number-type) (list string-type))
    (symbol-type `function))
  #t)
(check-equal? 
  (type-selector-selects?
    (arrow-type (list number-type) (list string-type))
    (symbol-type `not-function))
  #f)

(check-equal? 
  (type-selector-selects? 
    (field-type `foo void-type-body)
    (symbol-type `foo)) #t)
(check-equal? 
  (type-selector-selects? 
    (field-type `foo void-type-body)
    (symbol-type `not-foo)) #f)

(check-equal? 
  (type-selector-selects? 
    (field-type `number void-type-body)
    (field-type `field (struct-type-body (list (symbol-type `number))))) #t)
(check-equal? 
  (type-selector-selects? 
    (field-type `number void-type-body)
    (field-type `field (struct-type-body (list (symbol-type `not-number))))) #f)

