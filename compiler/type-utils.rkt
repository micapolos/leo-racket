#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/compiler/type
  leo/compiler/racket
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

(define (type-is-dynamic? ($type : Type)) : Boolean
  (cond
    ((racket? $type) #t)
    ((arrow? $type) #t)
    ((field? $type) (ormap type-is-dynamic? (field-type-stack $type)))
    ((a? $type) #f)))

(check-equal? (type-is-dynamic? (racket `number)) #t)
(check-equal? (type-is-dynamic? (arrow null (racket `foo))) #t)
(check-equal? (type-is-dynamic? (field `foo null)) #f)
(check-equal? (type-is-dynamic? (field `foo (list (field `foo null)))) #f)
(check-equal? (type-is-dynamic? (field `foo (list (racket `number)))) #t)
(check-equal? (type-is-dynamic? (field `foo (list (field `foo null) (racket `number)))) #t)
(check-equal? (type-is-dynamic? (a dynamic-type-a)) #f)

(define (type-stack-size ($type-stack : (Stackof Type))) : Exact-Nonnegative-Integer
  (length (filter type-is-dynamic? $type-stack)))

; -------------------------------------------------------------------------

(define (type-stack-dynamic-ref-from
  ($type-stack : (Stackof Type))
  ($index : Exact-Nonnegative-Integer)
  ($from-index : Exact-Nonnegative-Integer)
  ($from-dynamic-index : Exact-Nonnegative-Integer))
  : (Option Exact-Nonnegative-Integer)
  (cond
    ((null? $type-stack) #f)
    (else 
      (define $type (top $type-stack))
      (define $is-dynamic? (type-is-dynamic? $type))
      (cond
        ((= $from-index $index)
          (if $is-dynamic? $from-dynamic-index #f))
        (else 
          (type-stack-dynamic-ref-from
            (pop $type-stack)
            $index
            (+ $from-index 1)
            (if $is-dynamic? (+ $from-dynamic-index 1) $from-dynamic-index)))))))

(define (type-stack-dynamic-ref
  ($type-stack : (Stackof Type))
  ($index : Exact-Nonnegative-Integer))
  : (Option Exact-Nonnegative-Integer)
  (type-stack-dynamic-ref-from $type-stack $index 0 0))

(bind $type-stack (stack (racket `boolean) (racket `number) (field `foo null) (racket `string))
  (check-equal? (type-stack-dynamic-ref $type-stack 0) 0)
  (check-equal? (type-stack-dynamic-ref $type-stack 1) #f)
  (check-equal? (type-stack-dynamic-ref $type-stack 2) 1)
  (check-equal? (type-stack-dynamic-ref $type-stack 3) 2)
  (check-equal? (type-stack-dynamic-ref $type-stack 4) #f))

(define (type-check-symbol? ($type : Type) ($symbol : Symbol)) : Boolean
  (and
    (field? $type)
    (equal? (field-symbol $type) $symbol)))
