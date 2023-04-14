#lang leo/typed

(require
  racket/unsafe/ops
  leo/compiler/any-sexp
  leo/compiler/value
  leo/compiler/type
  leo/compiler/type-utils)

(define (type-sexp ($type : Type)) : Sexp
  (cond
    ((racket? $type) `racket)
    ;((equal? $type boolean-type) `boolean)
    ;((equal? $type number-type) `number)
    ;((equal? $type text-type) `text)
    ;((equal? $type int-type) `int)
    ;((equal? $type float-type) `float)
    ((field? $type) 
      (define $symbol (field-symbol $type))
      (define $structure (field-structure $type))
      (if (null? $structure)
        $symbol
        `(,$symbol ,@(structure-sexp-list $structure))))
    ((choice? $type)
      `(one (of ,@(structure-sexp-list (choice-type-stack $type)))))
    ((arrow? $type) 
      (define $lhs-structure (arrow-from-structure $type))
      (define $rhs-structure (arrow-to-structure $type))
      `(recipe 
        ,@(structure-sexp-list $lhs-structure)
        (doing ,@(structure-sexp-list $rhs-structure))))
    ((generic? $type) 
      `(generic ,(type-sexp (generic-type $type))))
    ((recursive? $type)
      `(recursive ,(type-sexp (recursive-type $type))))
    ((variable? $type) 
      `(variable ,(variable-index $type)))
    ((universe? $type) `(universe ,(universe-index $type)))
    ((reified? $type) `(reified ,@(structure-sexp-list (reified-structure $type))))))

(define (structure-sexp-list ($structure : Structure)) : (Listof Sexp)
  (reverse (map type-sexp $structure)))

(define (structure-sexp ($structure : Structure)) : Sexp
  `(structure ,@(structure-sexp-list $structure)))

(check-equal? (type-sexp (racket)) `racket)

; (check-equal? (type-sexp number-type) `number)
; (check-equal? (type-sexp text-type) `text)
; (check-equal? (type-sexp float-type) `float)
; (check-equal? (type-sexp int-type) `int)
; (check-equal? (type-sexp boolean-type) (type-sexp boolean-type))

(check-equal? (type-sexp (field `foo null)) `foo)
(check-equal? (type-sexp (field `foo (structure (racket)))) `(foo racket))

(check-equal? (type-sexp (choice!)) `(one (of)))
(check-equal? (type-sexp (choice! (field! `zero) (field! `one))) `(one (of zero one)))

(check-equal? (type-sexp (recursive (field! `foo))) `(recursive foo))

(check-equal? (type-sexp (generic (field! `foo))) `(generic foo))

(check-equal? (type-sexp (variable 0)) `(variable 0))

(check-equal? 
  (type-sexp 
    (field `foo (structure (field! `bar) (field! `zoo))))
  `(foo bar zoo))

(check-equal? 
  (type-sexp 
    (arrow 
      (stack number-type text-type)
      (stack text-type int-type)))
  (type-sexp (recipe! number-type text-type (doing text-type int-type))))

(check-equal? (type-sexp (universe 128)) `(universe 128))

(check-equal? (type-sexp (reified! (field! `foo))) `(reified foo))
