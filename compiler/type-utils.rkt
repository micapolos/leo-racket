#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/option
  leo/compiler/type
  leo/typed/base
  leo/typed/stack
  leo/typed/testing)

(define dynamic-type-a (racket-field `a))
(define dynamic-type-b (racket-field `b))
(define dynamic-type-c (racket-field `c))
(define dynamic-type-d (racket-field `d))

(define static-type-a (field `a null))
(define static-type-b (field `b null))
(define static-type-c (field `c null))
(define static-type-d (field `d null))

(define type-a dynamic-type-a)
(define type-b dynamic-type-b)
(define type-c dynamic-type-c)
(define type-d dynamic-type-d)

(define boolean-type (racket-field `boolean))
(define number-type (racket-field `number))
(define int-type (racket-field `int))
(define float-type (racket-field `float))
(define text-type (racket-field `text))
(define check-type (field `check (structure (choice (structure (field! `yes) (field! `no))))))

(define static-structure-a (structure static-type-a))
(define static-structure-b (structure static-type-b))
(define static-structure-c (structure static-type-c))
(define static-structure-d (structure static-type-d))

(define dynamic-structure-a (structure dynamic-type-a))
(define dynamic-structure-b (structure dynamic-type-b))
(define dynamic-structure-c (structure dynamic-type-c))
(define dynamic-structure-d (structure dynamic-type-d))

(define structure-a dynamic-structure-a)
(define structure-b dynamic-structure-b)
(define structure-c dynamic-structure-c)
(define structure-d dynamic-structure-d)

(define structure-ab (structure type-a type-b))
(define structure-cd (structure type-c type-d))

(define (type-dynamic? ($type : Type)) : Boolean
  (cond
    ((racket? $type) #t)
    ((arrow? $type) (structure-dynamic? (arrow-to-structure $type)))
    ((field? $type) (structure-dynamic? (field-structure $type)))
    ((choice? $type) (choice-dynamic? $type))
    ((generic? $type) #t)
    ((recursive? $type) #t)
    ((variable? $type) (error "impossible"))
    ((universe? $type) #t)
    ((value? $type) #t)))

(define (structure-dynamic? ($structure : Structure)) : Boolean
  (ormap type-dynamic? $structure))

(define (choice-dynamic? ($choice : Choice)) : Boolean
  (bind $structure (choice-type-stack $choice)
    (case (length $structure)
      ((0) #f)
      ((1) (type-dynamic? (top $structure)))
      (else #t))))

(check-equal? (type-dynamic? (racket)) #t)

(check-equal? (type-dynamic? (arrow static-structure-a static-structure-b)) #f)
(check-equal? (type-dynamic? (arrow dynamic-structure-a static-structure-b)) #f)
(check-equal? (type-dynamic? (arrow static-structure-a dynamic-structure-b)) #t)
(check-equal? (type-dynamic? (arrow dynamic-structure-a dynamic-structure-b)) #t)

(check-equal? (type-dynamic? (field `foo null)) #f)
(check-equal? (type-dynamic? (field `foo (structure (field `foo null)))) #f)
(check-equal? (type-dynamic? (racket-field `foo)) #t)
(check-equal? (type-dynamic? (field `foo (structure (field `foo null) (racket)))) #t)

(check-equal? (type-dynamic? (choice null-structure)) #f)
(check-equal? (type-dynamic? (choice (structure static-type-a))) #f)
(check-equal? (type-dynamic? (choice (structure dynamic-type-a))) #t)
(check-equal? (type-dynamic? (choice (structure static-type-a static-type-b))) #t)

(check-equal? (type-dynamic? (generic (field! `foo))) #t)
(check-equal? (type-dynamic? (generic (variable 0))) #t)

(check-equal? (type-dynamic? (recursive (field! `foo))) #t)
(check-equal? (type-dynamic? (recursive (variable 0))) #t)

(check-equal? (type-dynamic? (universe 0)) #t)

(check-equal? (type-dynamic? (value #f (field! `foo))) #t)

(define (structure-dynamic-size ($structure : Structure)) : Exact-Nonnegative-Integer
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

(bind $structure (stack boolean-type number-type (field `foo null) number-type)
  (check-equal? (structure-dynamic-ref $structure 0) 0)
  (check-equal? (structure-dynamic-ref $structure 1) #f)
  (check-equal? (structure-dynamic-ref $structure 2) 1)
  (check-equal? (structure-dynamic-ref $structure 3) 2)
  (check-equal? (structure-dynamic-ref $structure 4) #f))

(define (structure-dynamic-index 
  ($structure : Structure) 
  ($index : Exact-Nonnegative-Integer)) 
  : (Option Exact-Nonnegative-Integer)
  (option-bind (structure-dynamic-ref $structure $index) $ref
    (cast 
      (- (structure-dynamic-size $structure) $ref 1) Exact-Nonnegative-Integer)))

; ---------------------------------------------------------------------------

; TODO: Remove
(define (type-lift ($type : Type)) : (Option Type) #f)
(define (structure-lift ($structure : Structure)) : (Option Structure) #f)
