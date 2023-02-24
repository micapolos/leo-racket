#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/type
  leo/typed/types
  leo/typed/testing)

(define (type-any ($type : Type)) : Any
  (cond
    ((racket? $type) 
      (racket-any $type))
    ((symbol? $type) $type)
    ((giving? $type) 
      `(giving 
        ,@(map type-any (giving-lhs-types $type))
        ,@(map type-any (giving-rhs-types $type))))
    ((list? $type) (map type-any $type))
    ((any? $type)
      `(any ,(type-any (any-type $type))))
    ((thing? $type) `thing)
    (else $type)))

(check-equal? (type-any string-type) `string)
(check-equal? (type-any number-type) `number)
(check-equal? (type-any fixnum-type) `fixnum)
(check-equal? (type-any flonum-type) `flonum)
(check-equal? (type-any boolean-type) `boolean)
(check-equal? (type-any (racket `foo)) `foo)
(check-equal? (type-any `foo) `foo)
(check-equal? (type-any (thing)) `thing)

(check-equal? 
  (type-any `(foo ,number-type ,string-type))
  `(foo number string))

(check-equal? 
  (type-any (giving (list number-type string-type) (list boolean-type)))
  `(giving number string boolean))

(check-equal? (type-any (any number-type)) `(any number))
