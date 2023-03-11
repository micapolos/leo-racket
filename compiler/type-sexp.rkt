#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing
  leo/typed/stack
  leo/compiler/any-sexp
  leo/compiler/type
  leo/compiler/type-utils)

(define (type-sexp ($type : Type)) : Sexp
  (cond
    ((racket? $type) `racket)
    ((equal? $type number-type) `number)
    ((equal? $type text-type) `text)
    ((equal? $type int-type) `int)
    ((equal? $type float-type) `float)
    ((equal? $type boolean-type) `boolean)
    ((field? $type) 
      (define $symbol (field-symbol $type))
      (define $structure (field-structure $type))
      (if (null? $structure)
        $symbol
        `(,$symbol ,@(structure-sexp-list $structure))))
    ((choice? $type)
      `(choice ,@(structure-sexp-list (choice-structure $type))))
    ((arrow? $type) 
      (define $lhs-structure (arrow-lhs-structure $type))
      (define $rhs-structure (arrow-rhs-structure $type))
      `(recipe 
        ,@(structure-sexp-list $lhs-structure)
        (doing ,@(structure-sexp-list $rhs-structure))))
    ((a? $type) `(a ,(type-sexp (a-type $type))))
    ((generic? $type) 
      `(generic ,(type-sexp (generic-type $type))))
    ((recursive? $type) 
      `(recursive ,(type-sexp (recursive-type $type))))
    ((recurse? $type) 
      (bind $index (recurse-index $type)
        (if (= $index 0) 
          `recurse
          `(recurse (depth ,(add1 $index))))))))

(define (structure-sexp-list ($structure : Structure)) : (Listof Sexp)
  (reverse (map type-sexp $structure)))

(define (structure-sexp ($structure : Structure)) : Sexp
  `(structure ,@(structure-sexp-list $structure)))

(check-equal? (type-sexp (racket)) `racket)

(check-equal? (type-sexp number-type) `number)
(check-equal? (type-sexp text-type) `text)
(check-equal? (type-sexp float-type) `float)
(check-equal? (type-sexp int-type) `int)
(check-equal? (type-sexp boolean-type) `boolean)

(check-equal? (type-sexp (field `foo null)) `foo)
(check-equal? (type-sexp (field `foo (structure (racket)))) `(foo racket))

(check-equal? (type-sexp (choice null)) `(choice))
(check-equal? (type-sexp (choice (structure (racket)))) `(choice racket))

(check-equal? (type-sexp (recursive (null-field `foo))) `(recursive foo))
(check-equal? (type-sexp (generic (null-field `foo))) `(generic foo))

(check-equal? (type-sexp (recurse 0)) `recurse)
(check-equal? (type-sexp (recurse 128)) `(recurse (depth 129)))

(check-equal? 
  (type-sexp 
    (field `foo (structure (null-field `bar) (null-field `zoo))))
  `(foo bar zoo))

(check-equal? 
  (type-sexp 
    (arrow 
      (stack number-type text-type)
      (stack boolean-type int-type)))
  `(recipe number text (doing boolean int)))

(check-equal? 
  (type-sexp (a (racket)))
  `(a racket))
