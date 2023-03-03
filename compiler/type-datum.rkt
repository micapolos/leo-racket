#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/typed/stack
  leo/compiler/any-datum
  leo/compiler/racket
  leo/compiler/type)

(define (type-datum ($type : Type)) : Datum
  (cond
    ((racket? $type)
      (define $any (racket-any $type))
      (case $any
        ((boolean) `boolean)
        ((number) `number)
        ((fixnum) `fixnum)
        ((flonum) `flonum)
        ((string) `string)
        (else `(racket ,(any-datum $any)))))
    ((field? $type) 
      (define $symbol (field-symbol $type))
      (define $structure (field-structure $type))
      (if (null? $structure)
        $symbol
        `(,$symbol ,@(structure-datum-list $structure))))
    ((arrow? $type) 
      (define $lhs-structure (arrow-lhs-structure $type))
      (define $rhs-structure (arrow-rhs-structure $type))
      `(function 
        ,@(structure-datum-list $lhs-structure)
        (giving ,@(structure-datum-list $rhs-structure))))
    ((a? $type) `(a ,(type-datum (a-type $type))))))

(define (structure-datum-list ($structure : Structure)) : (Listof Datum)
  (reverse (map type-datum $structure)))

(check-equal? (type-datum (racket `void)) `(racket void))
(check-equal? (type-datum (racket `boolean)) `boolean)
(check-equal? (type-datum (racket `number)) `number)
(check-equal? (type-datum (racket `fixnum)) `fixnum)
(check-equal? (type-datum (racket `flonum)) `flonum)
(check-equal? (type-datum (racket `string)) `string)

(check-equal? (type-datum (racket `foo)) `(racket foo))
(check-equal? (type-datum (racket 123)) `(racket 123))
(check-equal? (type-datum (racket "foo")) `(racket "foo"))

(check-equal? (type-datum (field `foo null)) `foo)

(check-equal? (type-datum (field `foo (stack (racket `number)))) `(foo number))

(check-equal? 
  (type-datum 
    (field `foo (stack (racket `number) (racket `string)))) 
  `(foo number string))

(check-equal? 
  (type-datum 
    (arrow 
      (stack (racket `number) (racket `string))
      (stack (racket `boolean) (racket `fixnum)))) 
  `(function number string (giving boolean fixnum)))

(check-equal? (type-datum (a (racket `string))) `(a string))
