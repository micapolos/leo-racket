#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/typed/stack
  leo/compiler/any-sexp
  leo/compiler/racket
  leo/compiler/type)

(define (type-sexp ($type : Type)) : Sexp
  (cond
    ((racket? $type)
      (define $any (racket-any $type))
      (case $any
        ((boolean) `boolean)
        ((number) `number)
        ((fixnum) `fixnum)
        ((flonum) `flonum)
        ((string) `string)
        (else `(racket ,(any-sexp $any)))))
    ((field? $type) 
      (define $symbol (field-symbol $type))
      (define $structure (field-structure $type))
      (if (null? $structure)
        $symbol
        `(,$symbol ,@(structure-sexp-list $structure))))
    ((arrow? $type) 
      (define $lhs-structure (arrow-lhs-structure $type))
      (define $rhs-structure (arrow-rhs-structure $type))
      `(function 
        ,@(structure-sexp-list $lhs-structure)
        (giving ,@(structure-sexp-list $rhs-structure))))
    ((a? $type) `(a ,@(structure-sexp-list (a-structure $type))))))

(define (structure-sexp-list ($structure : Structure)) : (Listof Sexp)
  (reverse (map type-sexp $structure)))

(check-equal? (type-sexp (racket `void)) `(racket void))
(check-equal? (type-sexp (racket `boolean)) `boolean)
(check-equal? (type-sexp (racket `number)) `number)
(check-equal? (type-sexp (racket `fixnum)) `fixnum)
(check-equal? (type-sexp (racket `flonum)) `flonum)
(check-equal? (type-sexp (racket `string)) `string)

(check-equal? (type-sexp (racket `foo)) `(racket foo))
(check-equal? (type-sexp (racket 123)) `(racket 123))
(check-equal? (type-sexp (racket "foo")) `(racket "foo"))

(check-equal? (type-sexp (field `foo null)) `foo)

(check-equal? (type-sexp (field `foo (stack (racket `number)))) `(foo number))

(check-equal? 
  (type-sexp 
    (field `foo (stack (racket `number) (racket `string)))) 
  `(foo number string))

(check-equal? 
  (type-sexp 
    (arrow 
      (stack (racket `number) (racket `string))
      (stack (racket `boolean) (racket `fixnum)))) 
  `(function number string (giving boolean fixnum)))

(check-equal? 
  (type-sexp (a (structure (racket `string) (racket `number)))) 
  `(a string number))
