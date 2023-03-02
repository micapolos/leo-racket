#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/list
  racket/unsafe/ops
  leo/typed/testing
  leo/typed/stack
  leo/compiler/any-datum
  leo/compiler/type-datum
  leo/compiler/value
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/racket)

(define (value-datum ($value : Value)) : Datum
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
        (else (type-datum $type))))
    ((field? $type) 
      (define $symbol (field-symbol $type))
      (define $type-stack (field-type-stack $type))
      (cond
        ((null? $type-stack) $symbol)
        (else 
          `(
            ,$symbol
            ,@(reverse
              (map
                (lambda (($index : Exact-Nonnegative-Integer))
                  (value-datum (any-type-stack-ref $any $type-stack $index)))
                (range (length $type-stack))))))))
    ((arrow? $type) (type-datum $type))
    ((a? $type) `(a ,(type-datum (a-type $type))))))

(define (any-type-stack-ref
  ($any : Any)
  ($type-stack : (Stackof Type))
  ($index : Exact-Nonnegative-Integer))
  : Value
  (define $type-stack-size (type-stack-size $type-stack))
  (define $dynamic-index (type-stack-dynamic-ref $type-stack $index))
  (value 
    (and
      $dynamic-index
      (case $type-stack-size
        ((0) (error "impossible"))
        ((1) $any)
        ((2)
          ((if (= $dynamic-index 1) unsafe-car unsafe-cdr) (cast $any (Pairof Any Any))))
        (else
          (unsafe-vector-ref 
            (cast $any (Vectorof Any))
            (- $type-stack-size $dynamic-index 1)))))
    (list-ref $type-stack $index)))

(check-equal?
  (value-datum (value #t (racket `boolean)))
  `(boolean true))

(check-equal?
  (value-datum (value #f (racket `boolean)))
  `(boolean false))

(check-equal?
  (value-datum (value 3.14 (racket `number)))
  3.14)

(check-equal?
  (value-datum (value 1 (racket `fixnum)))
  `(fixnum 1))

(check-equal?
  (value-datum (value 3.14 (racket `flonum)))
  `(flonum 3.14))

(check-equal?
  (value-datum (value "foo" (racket `string)))
  "foo")

(check-equal?
  (value-datum (value `foo (racket `bar)))
  `(racket bar))

(check-equal?
  (value-datum 
    (value `foo 
      (arrow 
        (stack (racket `string)) 
        (stack (racket `number)))))
  `(function string (giving number)))

(check-equal?
  (value-datum (value `foo (a (racket `number))))
  `(a number))

(check-equal?
  (value-datum (value "foo" (field `foo (stack (racket `string)))))
  `(foo "foo"))

(check-equal?
  (value-datum 
    (value 
      (cons 128 "foo") 
      (field `foo (stack (racket `number) (field `bar null) (racket `string)))))
  `(foo 128 bar "foo"))

(check-equal?
  (value-datum 
    (value 
      (vector 128 "foo" #t) 
      (field `foo (stack (racket `number) (field `bar null) (racket `string) (racket `boolean)))))
  `(foo 128 bar "foo" (boolean true)))
