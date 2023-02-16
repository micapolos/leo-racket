#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/types
  leo/typed/type-select
  leo/testing)

(define (struct-type-body-get-indexed 
    ($struct-type-body : StructTypeBody)
    ($selector : Type))
  (type-list-selector-indexed 
    (struct-type-body-type-list $struct-type-body) 
    $selector))

(define (type-body-get-indexed 
    ($type-body : TypeBody)
    ($selector : Type))
  (cond
    ((struct-type-body? $type-body) 
      (struct-type-body-get-indexed $type-body $selector))
    ((choice-type-body? $type-body) #f)))

(define (field-type-get-indexed 
    ($field-type : FieldType)
    ($selector : Type))
  (type-body-get-indexed (field-type-body $field-type) $selector))

(define (type-get-indexed 
    ($type : Type)
    ($selector : Type))
  (cond
    ((native-type? $type) #f)
    ((symbol-type? $type) #f)
    ((field-type? $type) (field-type-get-indexed $type $selector))
    ((arrow-type? $type) #f)))

(check-equal?
  (type-get-indexed
    (field-type `foo
      (struct-type-body 
        (list number-type (symbol-type `foo) string-type)))
    string-type)
  (cons 1 string-type))
