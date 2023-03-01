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
      (define $type-stack (field-type-stack $type))
      (if (null? $type-stack)
        $symbol
        `(,$symbol ,@(type-stack-datum-list $type-stack))))
    ((arrow? $type) 
      (define $lhs-type-stack (arrow-lhs-type-stack $type))
      (define $rhs-type (arrow-rhs-type $type))
      `(function 
        ,@(type-stack-datum-list $lhs-type-stack)
        (giving ,(type-datum $rhs-type))))
    ((a? $type) `(a ,(type-datum (a-type $type))))))

(define (type-stack-datum-list ($type-stack : (Stackof Type))) : (Listof Datum)
  (reverse (map type-datum $type-stack)))

(check-equal? (type-datum (racket `void)) `void)
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
      (racket `boolean))) 
  `(function number string (giving boolean)))

(check-equal? (type-datum (a (racket `string))) `(a string))
