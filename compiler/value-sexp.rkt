#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/list
  racket/unsafe/ops
  leo/typed/testing
  leo/typed/stack
  leo/compiler/any-sexp
  leo/compiler/type-sexp
  leo/compiler/value
  leo/compiler/type
  leo/compiler/type-utils)

(define (value-sexp ($value : Value)) : Sexp
  (define $type (value-type $value))
  (define $any (value-any $value))
  (cond
    ((racket? $type) (any-sexp $any))
    ((equal? $type int-type) `(int ,(cast $any Fixnum)))
    ((equal? $type float-type) `(float ,(cast $any Flonum)))
    ((equal? $type number-type) (cast $any Number))
    ((equal? $type boolean-type) `(boolean ,(if (cast $any Boolean) `true `false)))
    ((equal? $type text-type) (cast $any String))
    ((field? $type) 
      (define $symbol (field-symbol $type))
      (define $structure (field-structure $type))
      (cond
        ((null? $structure) $symbol)
        (else 
          `(
            ,$symbol
            ,@(any-structure-sexp-list $any $structure)))))
    ((choice? $type) (error "TODO (value-sexp choice)"))
    ((arrow? $type) (type-sexp $type))
    ((a? $type) (type-sexp (a-type $type)))))

(define (any-structure-sexp-list ($any : Any) ($structure : Structure)) : (Listof Sexp)
  (reverse
    (map
      (lambda (($index : Exact-Nonnegative-Integer))
        (value-sexp (any-structure-ref $any $structure $index)))
      (range (length $structure)))))

(define (any-structure-ref
  ($any : Any)
  ($structure : Structure)
  ($index : Exact-Nonnegative-Integer))
  : Value
  (define $structure-compiled-size (structure-compiled-size $structure))
  (define $dynamic-index (structure-dynamic-ref $structure $index))
  (value 
    (and
      $dynamic-index
      (case $structure-compiled-size
        ((0) (error "impossible"))
        ((1) $any)
        ((2)
          ((if (= $dynamic-index 1) unsafe-car unsafe-cdr) (cast $any (Pairof Any Any))))
        (else
          (unsafe-vector-ref 
            (cast $any (Vectorof Any))
            (- $structure-compiled-size $dynamic-index 1)))))
    (list-ref $structure $index)))

(check-equal?
  (value-sexp (value #t boolean-type))
  `(boolean true))

(check-equal?
  (value-sexp (value #f boolean-type))
  `(boolean false))

(check-equal?
  (value-sexp (value 3.14 number-type))
  3.14)

(check-equal?
  (value-sexp (value 1 int-type))
  `(int 1))

(check-equal?
  (value-sexp (value 3.14 float-type))
  `(float 3.14))

(check-equal?
  (value-sexp (value "foo" text-type))
  "foo")

(check-equal?
  (value-sexp (value `(quote 1 2 3) (racket)))
  `(quote 1 2 3))

(check-equal?
  (value-sexp 
    (value `foo 
      (arrow 
        (stack text-type) 
        (stack number-type))))
  `(recipe text (doing number)))

(check-equal?
  (value-sexp (value `foo (a number-type)))
  `number)

(check-equal?
  (value-sexp (value "foo" (field `foo (stack text-type))))
  `(foo "foo"))

(check-equal?
  (value-sexp 
    (value 
      (cons 128 "foo") 
      (field `foo (stack number-type (null-field `bar) text-type))))
  `(foo 128 bar "foo"))

(check-equal?
  (value-sexp 
    (value 
      (vector 128 "foo" #t) 
      (field `foo (stack number-type (null-field `bar) text-type boolean-type))))
  `(foo 128 bar "foo" (boolean true)))
