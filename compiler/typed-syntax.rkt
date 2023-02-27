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

(define (typed-syntax-field-ref
  ($typed-syntax-field : (Typed Syntax Field))
  ($index : Exact-Nonnegative-Integer))
