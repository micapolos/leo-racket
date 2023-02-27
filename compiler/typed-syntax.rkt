#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/base
  leo/typed/testing
  leo/compiler/racket
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/typed)

(define (typed-syntax->typed-sexp ($typed-syntax : (Typed Syntax Type))) : (Typed Sexp Type)
  (typed (syntax->datum (typed-value $typed-syntax)) (typed-type $typed-syntax)))

(define (typed-syntax-is-dynamic? ($typed-syntax : (Typed Syntax Type))) : Boolean
  (type-is-dynamic? (typed-type $typed-syntax)))

; ------------------------------------------------------------------

(define (typed-syntax-stack->syntax
  ($typed-syntax-stack : (Stackof (Typed Syntax Type))))
  : Syntax
  (define $dynamic-typed-syntax-stack 
    (filter typed-syntax-is-dynamic? $typed-syntax-stack))
  (define $dynamic-syntax-stack
    (map (ann typed-value (-> (Typed Syntax Type) Syntax)) $dynamic-typed-syntax-stack))
  (define $dynamic-length 
    (length $dynamic-syntax-stack))
  (case $dynamic-length
    ((0) (datum->syntax #f #f))
    ((1) (top $dynamic-syntax-stack))
    ((2) 
      (datum->syntax #f 
        `(cons ,(pop-top $dynamic-syntax-stack) ,(top $dynamic-syntax-stack))))
    (else 
      (datum->syntax #f 
        `(vector ,@(reverse $dynamic-syntax-stack))))))

(check-equal?
  (syntax->datum
    (typed-syntax-stack->syntax 
      (stack
        (typed #`a (field `foo null)))))
  #f)

(check-equal?
  (syntax->datum
    (typed-syntax-stack->syntax 
      (stack
        (typed #`a (field `foo null))
        (typed #`b (racket `number)))))
  `b)

(check-equal?
  (syntax->datum
    (typed-syntax-stack->syntax 
      (stack
        (typed #`a (field `foo null))
        (typed #`b (racket `number))
        (typed #`c (racket `string)))))
  `(cons b c))

(check-equal?
  (syntax->datum
    (typed-syntax-stack->syntax 
      (stack
        (typed #`a (field `foo null))
        (typed #`b (racket `number))
        (typed #`c (racket `string))
        (typed #`d (racket `boolean)))))
  `(vector b c d))

; ------------------------------------------------------------------

(define (typed-syntax-stack-make
  ($typed-syntax-stack : (Stackof (Typed Syntax Type)))
  ($symbol : Symbol))
  : (Typed Syntax Field)
  (typed
    (typed-syntax-stack->syntax $typed-syntax-stack)
    (field $symbol 
      (map 
        (ann typed-type (-> (Typed Syntax Type) Type)) 
        $typed-syntax-stack))))

(bind $typed-syntax
  (typed-syntax-stack-make
    (stack
      (typed #`a (field `foo null))
      (typed #`b (racket `number))
      (typed #`c (racket `string)))
    `tuple)
  (check-equal? 
    (typed-type $typed-syntax) 
    (field `tuple 
      (stack 
        (field `foo null)
        (racket `number)
        (racket `string))))
  (check-equal? 
    (syntax->datum (typed-value $typed-syntax))
    `(cons b c)))

; -------------------------------------------------------------------

(define (syntax-type-stack-ref
  ($syntax : Syntax)
  ($type-stack : (Stackof Type))
  ($index : Exact-Nonnegative-Integer))
  : (Typed Syntax Type)
  (define $type-stack-size (type-stack-size $type-stack))
  (define $dynamic-index (type-stack-dynamic-ref $type-stack $index))
  (typed 
    (datum->syntax #f
      (and
        $dynamic-index
        (case $type-stack-size
          ((0) (error "impossible"))
          ((1) $syntax)
          ((2)
            `(,(if (= $dynamic-index 1) `unsafe-car `unsafe-cdr) ,$syntax))
          (else
            `(unsafe-vector-ref 
              ,$syntax
              ,(- $type-stack-size $dynamic-index 1))))))
    (list-ref $type-stack $index)))

(check-equal? 
  (typed-syntax->typed-sexp 
    (syntax-type-stack-ref 
      #`a 
      (stack (field `t2 null))
      0)) 
  (typed #f (field `t2 null)))

(check-equal? 
  (typed-syntax->typed-sexp 
    (syntax-type-stack-ref 
      #`a 
      (stack (racket `t1) (field `t2 null))
      0)) 
  (typed #f (field `t2 null)))

(check-equal? 
  (typed-syntax->typed-sexp 
    (syntax-type-stack-ref 
      #`a 
      (stack (racket `t1) (field `t2 null))
      1)) 
  (typed `a (racket `t1)))

(check-equal? 
  (typed-syntax->typed-sexp 
    (syntax-type-stack-ref 
      #`a 
      (stack (racket `t1) (field `t2 null) (racket `t3))
      0)) 
  (typed `(unsafe-cdr a) (racket `t3)))

(check-equal? 
  (typed-syntax->typed-sexp 
    (syntax-type-stack-ref 
      #`a 
      (stack (racket `t1) (field `t2 null) (racket `t3))
      1)) 
  (typed #f (field `t2 null)))

(check-equal? 
  (typed-syntax->typed-sexp 
    (syntax-type-stack-ref 
      #`a 
      (stack (racket `t1) (field `t2 null) (racket `t3))
      2)) 
  (typed `(unsafe-car a) (racket `t1)))

(check-equal? 
  (typed-syntax->typed-sexp 
    (syntax-type-stack-ref 
      #`a 
      (stack (racket `t1) (racket `t2) (field `t3 null) (racket `t4))
      0)) 
  (typed `(unsafe-vector-ref a 2) (racket `t4)))

(check-equal? 
  (typed-syntax->typed-sexp 
    (syntax-type-stack-ref 
      #`a 
      (stack (racket `t1) (racket `t2) (field `t3 null) (racket `t4))
      1)) 
  (typed #f (field `t3 null) ))

(check-equal? 
  (typed-syntax->typed-sexp 
    (syntax-type-stack-ref 
      #`a 
      (stack (racket `t1) (racket `t2) (field `t3 null) (racket `t4))
      2)) 
  (typed `(unsafe-vector-ref a 1) (racket `t2)))

(check-equal? 
  (typed-syntax->typed-sexp 
    (syntax-type-stack-ref 
      #`a 
      (stack (racket `t1) (racket `t2) (field `t3 null) (racket `t4))
      3)) 
  (typed `(unsafe-vector-ref a 0) (racket `t1)))
