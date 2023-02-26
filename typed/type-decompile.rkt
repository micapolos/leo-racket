#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/type
  leo/typed/types
  leo/typed/testing)

(define (type-decompile ($type : Type)) : Any
  (cond
    ((racket? $type) 
      (racket-any $type))
    ((symbol? $type) $type)
    ((arrow? $type) 
      `(giving 
        ,@(map type-decompile (arrow-lhs-types $type))
        ,(type-decompile (arrow-rhs-type $type))))
    ((list? $type) (map type-decompile $type))
    ((any? $type)
      `(any ,(type-decompile (any-type $type))))
    ((thing? $type) `thing)
    (else $type)))

(check-equal? (type-decompile string-type) `string)
(check-equal? (type-decompile number-type) `number)
(check-equal? (type-decompile fixnum-type) `fixnum)
(check-equal? (type-decompile flonum-type) `flonum)
(check-equal? (type-decompile boolean-type) `boolean)
(check-equal? (type-decompile (racket `foo)) `foo)
(check-equal? (type-decompile `foo) `foo)
(check-equal? (type-decompile (thing)) `thing)

(check-equal? 
  (type-decompile `(foo ,number-type ,string-type))
  `(foo number string))

(check-equal? 
  (type-decompile (arrow (list number-type string-type) boolean-type))
  `(giving number string boolean))

(check-equal? (type-decompile (any number-type)) `(any number))
