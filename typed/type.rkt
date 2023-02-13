#lang typed/racket/base

(provide (all-defined-out))

(define-type Type
  (U 
    NativeType
    FieldType
    ArrowType))

(struct native-type ((any : Any))
  #:type-name NativeType
  #:transparent)

(struct field-type ((symbol : Symbol) (body : TypeBody))
  #:transparent
  #:type-name FieldType)

(struct arrow-type ((lhs : Type) (rhs : Type))
  #:transparent
  #:type-name ArrowType)

(define-type TypeBody (U StructTypeBody ChoiceTypeBody))

(struct struct-type-body ((type-lines : (Listof Type)))
  #:transparent
  #:type-name StructTypeBody)

(struct choice-type-body ((type-lines : (Listof Type)))
  #:transparent
  #:type-name ChoiceTypeBody)

(define boolean-type (native-type `boolean))
(define number-type (native-type `number))
(define string-type (native-type `string))
