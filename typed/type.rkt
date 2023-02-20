#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/testing)

(define-type Type
  (U 
    NativeType
    SymbolType
    FieldType
    ArrowType))

(struct native-type ((any : Any))
  #:type-name NativeType
  #:transparent)

(struct symbol-type ((symbol : Symbol))
  #:transparent
  #:type-name SymbolType)

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

(define (type-arrow $type) : ArrowType
  (cond
    ((arrow-type? $type) $type)
    (else (error (format "not arrow ~v" $type)))))

(define (type-field $type) : FieldType
  (cond
    ((field-type? $type) $type)
    (else (error (format "not field ~v" $type)))))
