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
      `(racket ,(any-sexp (racket-any $type))))
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
    ((a? $type) `(a ,(type-sexp (a-type $type))))))

(define (structure-sexp-list ($structure : Structure)) : (Listof Sexp)
  (reverse (map type-sexp $structure)))

(define (structure-sexp ($structure : Structure)) : Sexp
  `(structure ,@(structure-sexp-list $structure)))

(check-equal? (type-sexp (racket `foo)) `(racket foo))

(check-equal? (type-sexp (racket `foo)) `(racket foo))
(check-equal? (type-sexp (racket 123)) `(racket 123))
(check-equal? (type-sexp (racket "foo")) `(racket "foo"))

(check-equal? (type-sexp (field `foo null)) `foo)

(check-equal? (type-sexp (field `foo (stack (racket `number)))) `(foo (racket number)))

(check-equal? 
  (type-sexp 
    (field `foo (stack (racket `number) (racket `string)))) 
  `(foo (racket number) (racket string)))

(check-equal? 
  (type-sexp 
    (arrow 
      (stack (racket `number) (racket `string))
      (stack (racket `boolean) (racket `fixnum)))) 
  `(function (racket number) (racket string) (giving (racket boolean) (racket fixnum))))

(check-equal? 
  (type-sexp (a (racket `string)))
  `(a (racket string)))
