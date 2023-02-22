#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/type
  leo/typed/types
  leo/typed/testing)

(define (type-any ($type : Type)) : Any
  (cond
    ((native-type? $type) 
      (native-type-any $type))
    ((symbol-type? $type) 
      (symbol-type-symbol $type))
    ((arrow-type? $type) 
      `(giving 
        ,@(map type-any (arrow-type-lhs-types $type))
        ,@(map type-any (arrow-type-rhs-types $type))))
    ((field-type? $type)
      `(,(field-type-symbol $type)
        ,@(type-body-anys (field-type-body $type))))))

(define (type-body-anys ($type-body : TypeBody)) : (Listof Any)
  (cond
    ((struct-type-body? $type-body)
      (map type-any (struct-type-body-type-list $type-body)))
    ((choice-type-body? $type-body) 
      (error "TODO: (type-body-anys choice-type)"))))

(check-equal? (type-any string-type) `string)
(check-equal? (type-any number-type) `number)
(check-equal? (type-any fixnum-type) `fixnum)
(check-equal? (type-any flonum-type) `flonum)
(check-equal? (type-any boolean-type) `boolean)
(check-equal? (type-any (native-type `foo)) `foo)
(check-equal? (type-any (symbol-type `foo)) `foo)

(check-equal? 
  (type-any 
    (field-type `foo 
      (struct-type-body (list number-type string-type))) )
  `(foo number string))

(check-equal? 
  (type-any (arrow-type (list number-type string-type) (list boolean-type)))
  `(giving number string boolean))
