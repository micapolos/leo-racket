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
    ((symbol? $type) $type)
    ((arrow-type? $type) 
      `(giving 
        ,@(map type-any (arrow-type-lhs-types $type))
        ,@(map type-any (arrow-type-rhs-types $type))))
    ((list? $type) (map type-any $type))
    ((type-type? $type)
      `(any ,(type-any (type-type-type $type))))
    ((thing-type? $type) `thing)
    (else $type)))

(check-equal? (type-any string-type) `string)
(check-equal? (type-any number-type) `number)
(check-equal? (type-any fixnum-type) `fixnum)
(check-equal? (type-any flonum-type) `flonum)
(check-equal? (type-any boolean-type) `boolean)
(check-equal? (type-any (native-type `foo)) `foo)
(check-equal? (type-any `foo) `foo)
(check-equal? (type-any (thing-type)) `thing)

(check-equal? 
  (type-any `(foo ,number-type ,string-type))
  `(foo number string))

(check-equal? 
  (type-any (arrow-type (list number-type string-type) (list boolean-type)))
  `(giving number string boolean))

(check-equal? (type-any (type-type number-type)) `(any number))
