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
  leo/compiler/type-utils
  leo/compiler/racket)

(define (value-sexp ($value : Value)) : Sexp
  (define $type (value-type $value))
  (define $any (value-any $value))
  (cond
    ((racket? $type)
      (define $type-any (racket-any $type))
      (cond
        ((equal? $type-any `boolean) 
          `(boolean ,(if (cast $any Boolean) `true `false)))
        ((equal? $type-any `number) 
          (cast $any Number))
        ((equal? $type-any `fixnum) 
          `(fixnum ,(cast $any Fixnum)))
        ((equal? $type-any `flonum) 
          `(flonum ,(cast $any Flonum)))
        ((equal? $type-any `string) 
          (cast $any String))
        (else (type-sexp $type))))
    ((equal? $type int-type) (cast $any Fixnum))
    ((equal? $type float-type) (cast $any Flonum))
    ((equal? $type number-type) (cast $any Number))
    ((equal? $type text-type) (cast $any String))
    ((field? $type) 
      (define $symbol (field-symbol $type))
      (define $structure (field-structure $type))
      (cond
        ((null? $structure) $symbol)
        (else 
          `(
            ,$symbol
            ,@(reverse
              (map
                (lambda (($index : Exact-Nonnegative-Integer))
                  (value-sexp (any-structure-ref $any $structure $index)))
                (range (length $structure))))))))
    ((arrow? $type) (type-sexp $type))
    ((a? $type) `(a ,(type-sexp (a-type $type))))))

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
  (value-sexp (value #t (racket `boolean)))
  `(boolean true))

(check-equal?
  (value-sexp (value #f (racket `boolean)))
  `(boolean false))

(check-equal?
  (value-sexp (value 3.14 (racket `number)))
  3.14)

(check-equal?
  (value-sexp (value 1 (racket `fixnum)))
  `(fixnum 1))

(check-equal?
  (value-sexp (value 3.14 (racket `flonum)))
  `(flonum 3.14))

(check-equal?
  (value-sexp (value "foo" (racket `string)))
  "foo")

(check-equal?
  (value-sexp (value `foo (racket `bar)))
  `(racket bar))

(check-equal?
  (value-sexp 
    (value `foo 
      (arrow 
        (stack (racket `string)) 
        (stack (racket `number)))))
  `(function string (giving number)))

(check-equal?
  (value-sexp (value `foo (a (racket `number))))
  `(a number))

(check-equal?
  (value-sexp (value "foo" (field `foo (stack (racket `string)))))
  `(foo "foo"))

(check-equal?
  (value-sexp 
    (value 
      (cons 128 "foo") 
      (field `foo (stack (racket `number) (field `bar null) (racket `string)))))
  `(foo 128 bar "foo"))

(check-equal?
  (value-sexp 
    (value 
      (vector 128 "foo" #t) 
      (field `foo (stack (racket `number) (field `bar null) (racket `string) (racket `boolean)))))
  `(foo 128 bar "foo" (boolean true)))