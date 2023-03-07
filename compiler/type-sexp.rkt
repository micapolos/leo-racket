#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/typed/stack
  leo/compiler/any-sexp
  leo/compiler/type
  leo/compiler/type-utils)

(define (type-sexp ($type : Type)) : Sexp
  (cond
    ((racket? $type) `racket)
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

(check-equal? (type-sexp (racket)) `racket)

(check-equal? (type-sexp (field `foo null)) `foo)

(check-equal? (type-sexp (field `foo (structure (racket)))) `(foo racket))

(check-equal? 
  (type-sexp 
    (field `foo (structure (null-field `bar) (null-field `zoo))))
  `(foo bar zoo))

(check-equal? 
  (type-sexp 
    (arrow 
      (stack (null-field `number) (null-field `string))
      (stack (null-field `boolean) (null-field `fixnum))))
  `(function number string (giving boolean fixnum)))

(check-equal? 
  (type-sexp (a (racket)))
  `(a racket))
