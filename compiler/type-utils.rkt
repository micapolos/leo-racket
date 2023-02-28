#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/compiler/indexed
  leo/compiler/type
  leo/compiler/racket
  leo/typed/base
  leo/typed/stack
  leo/typed/testing)

(define (type-is-dynamic? ($type : Type)) : Boolean
  (cond
    ((racket? $type) #t)
    ((arrow? $type) #t)
    ((field? $type) (ormap type-is-dynamic? (field-type-stack $type)))))

(check-equal? (type-is-dynamic? (racket `number)) #t)
(check-equal? (type-is-dynamic? (arrow null (racket `foo))) #t)
(check-equal? (type-is-dynamic? (field `foo null)) #f)
(check-equal? (type-is-dynamic? (field `foo (list (field `foo null)))) #f)
(check-equal? (type-is-dynamic? (field `foo (list (racket `number)))) #t)
(check-equal? (type-is-dynamic? (field `foo (list (field `foo null) (racket `number)))) #t)

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
