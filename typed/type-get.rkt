#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/types
  leo/typed/type-select
  leo/testing)

(define (field-type-get 
    ($field-type : FieldType)
    ($selector : Type))
  (type-body-select (field-type-body $field-type) $selector))

(define (type-get 
    ($type : Type)
    ($selector : Type))
  (cond
    ((native-type? $type) #f)
    ((symbol-type? $type) #f)
    ((field-type? $type) (field-type-get $type $selector))
    ((arrow-type? $type) #f)))

(check-equal?
  (type-get
    (field-type `foo
      (struct-type-body 
        (list number-type (symbol-type `foo) string-type)))
    string-type)
  (cons 1 string-type))
