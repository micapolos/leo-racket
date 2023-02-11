#lang typed/racket/base

(provide (all-defined-out))

(define-type TypeLine
  (U 
    BooleanTypeLine
    NumberTypeLine
    StringTypeLine
    FieldTypeLine
    ArrowTypeLine))

(struct boolean-type-line () 
  #:type-name BooleanTypeLine
  #:transparent)

(struct number-type-line () 
  #:type-name NumberTypeLine
  #:transparent)

(struct string-type-line () 
  #:type-name StringTypeLine
  #:transparent)

(struct field-type-line ((symbol : Symbol) (body : Type))
  #:transparent
  #:type-name FieldTypeLine)

(struct arrow-type-line ((lhs : Type) (rhs : Type))
  #:transparent
  #:type-name ArrowTypeLine)

(define-type Type (U StructType ChoiceType))

(struct struct-type ((type-lines : (Listof TypeLine)))
  #:transparent
  #:type-name StructType)

(struct choice-type ((type-lines : (Listof TypeLine)))
  #:transparent
  #:type-name ChoiceType)

(define boolean-type (struct-type (list (boolean-type-line))))
(define number-type (struct-type (list (number-type-line))))
(define string-type (struct-type (list (string-type-line))))
