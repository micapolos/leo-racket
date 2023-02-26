#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/type
  leo/typed/types
  leo/typed/testing)

(define (type-decompile ($type : Type)) : Any
  (cond
    ((racket? $type) (racket-any $type))
    ((arrow? $type) 
      `(giving 
        ,@(map type-decompile (arrow-lhs-types $type))
        ,(type-decompile (arrow-rhs-type $type))))
    ((tuple? $type) 
      (let (($symbol (tuple-symbol $type))
            ($type-list (tuple-type-list $type)))
      (cond 
        ((null? $type-list) $symbol)
        (else 
          `(
            ,$symbol
            ,@(map type-decompile $type-list))))))
    ((any? $type)
      `(any ,(type-decompile (any-type $type))))
    ((thing? $type) `thing)))

(check-equal? (type-decompile string-type) `string)
(check-equal? (type-decompile number-type) `number)
(check-equal? (type-decompile fixnum-type) `fixnum)
(check-equal? (type-decompile flonum-type) `flonum)
(check-equal? (type-decompile boolean-type) `boolean)
(check-equal? (type-decompile (racket `foo)) `foo)
(check-equal? (type-decompile (thing)) `thing)

(check-equal? 
  (type-decompile (tuple `foo null)) 
  `foo)

(check-equal? 
  (type-decompile (tuple `foo (list number-type string-type)))
  `(foo number string))

(check-equal? 
  (type-decompile (arrow (list number-type string-type) boolean-type))
  `(giving number string boolean))

(check-equal? (type-decompile (any number-type)) `(any number))
