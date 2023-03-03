#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/compiler/type
  leo/compiler/racket
  leo/compiler/racket-utils
  leo/typed/base
  leo/typed/stack
  leo/typed/testing)

(define dynamic-type-a (racket `a))
(define dynamic-type-b (racket `b))
(define dynamic-type-c (racket `c))
(define dynamic-type-d (racket `d))

(define static-type-a (field `a null))
(define static-type-b (field `b null))
(define static-type-c (field `c null))
(define static-type-d (field `d null))

(define type-a dynamic-type-a)
(define type-b dynamic-type-b)
(define type-c dynamic-type-c)
(define type-d dynamic-type-d)

(define boolean-type (field `boolean (stack boolean-racket)))
(define number-type (field `number (stack number-racket)))
(define int-type (field `int (stack fixnum-racket)))
(define float-type (field `float (stack flonum-racket)))
(define text-type (field `text (stack string-racket)))

(define (type-dynamic? ($type : Type)) : Boolean
  (cond
    ((racket? $type) #t)
    ((arrow? $type) #t)
    ((field? $type) (ormap type-dynamic? (field-structure $type)))
    ((a? $type) #f)))

(check-equal? (type-dynamic? (racket `number)) #t)
(check-equal? (type-dynamic? (arrow null null)) #t)
(check-equal? (type-dynamic? (field `foo null)) #f)
(check-equal? (type-dynamic? (field `foo (structure (field `foo null)))) #f)
(check-equal? (type-dynamic? (field `foo (structure (racket `number)))) #t)
(check-equal? (type-dynamic? (field `foo (structure (field `foo null) (racket `number)))) #t)
(check-equal? (type-dynamic? (a (structure dynamic-type-a))) #f)

(define (structure-size ($structure : Structure)) : Exact-Nonnegative-Integer
  (length (filter type-dynamic? $structure)))

; -------------------------------------------------------------------------

(define (structure-dynamic-ref-from
  ($structure : Structure)
  ($index : Exact-Nonnegative-Integer)
  ($from-index : Exact-Nonnegative-Integer)
  ($from-dynamic-index : Exact-Nonnegative-Integer))
  : (Option Exact-Nonnegative-Integer)
  (cond
    ((null? $structure) #f)
    (else 
      (define $type (top $structure))
      (define $is-dynamic? (type-dynamic? $type))
      (cond
        ((= $from-index $index)
          (if $is-dynamic? $from-dynamic-index #f))
        (else 
          (structure-dynamic-ref-from
            (pop $structure)
            $index
            (+ $from-index 1)
            (if $is-dynamic? (+ $from-dynamic-index 1) $from-dynamic-index)))))))

(define (structure-dynamic-ref
  ($structure : Structure)
  ($index : Exact-Nonnegative-Integer))
  : (Option Exact-Nonnegative-Integer)
  (structure-dynamic-ref-from $structure $index 0 0))

(bind $structure (stack (racket `boolean) (racket `number) (field `foo null) (racket `string))
  (check-equal? (structure-dynamic-ref $structure 0) 0)
  (check-equal? (structure-dynamic-ref $structure 1) #f)
  (check-equal? (structure-dynamic-ref $structure 2) 1)
  (check-equal? (structure-dynamic-ref $structure 3) 2)
  (check-equal? (structure-dynamic-ref $structure 4) #f))

(define (type-check-symbol? ($type : Type) ($symbol : Symbol)) : Boolean
  (and
    (field? $type)
    (equal? (field-symbol $type) $symbol)))
