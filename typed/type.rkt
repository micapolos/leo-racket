#lang typed/racket/base

(provide (all-defined-out))

(require leo/testing)

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

(struct arrow-type ((lhs-types : (Listof Type)) (rhs-types : (Listof Type)))
  #:transparent
  #:type-name ArrowType)

(define-type TypeBody (U StructTypeBody ChoiceTypeBody))

(struct struct-type-body ((type-list : (Listof Type)))
  #:transparent
  #:type-name StructTypeBody)

(struct choice-type-body ((type-list : (Listof Type)))
  #:transparent
  #:type-name ChoiceTypeBody)
