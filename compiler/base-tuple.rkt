#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/math
  racket/unsafe/ops
  leo/compiler/expression
  leo/compiler/base-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define assembly-tuple
  (tuple
    (unary-expression boolean-type `disassemble (choice! (field! `true) (field! `false)) #`identity)
    (binary-expression (choice! (field! `true) (field! `false)) `assemble (field! `boolean) boolean-type #`identity)))

(define base-type-tuple
  (tuple
    (symbol-expression `number type-type #`number-type)
    (symbol-expression `int type-type #`int-type)
    (symbol-expression `float type-type #`float-type)
    (symbol-expression `text type-type #`text-type)))

(define base-value-tuple
  (tuple
    (unit-expression-2 `boolean `true boolean-type #`(lambda () #t))
    (unit-expression-2 `boolean `false boolean-type #`(lambda () #f))

    (binary-expression number-type `plus number-type number-type #`+)
    (binary-expression number-type `minus number-type number-type #`-)
    (binary-expression number-type `times number-type number-type #`*)
    (binary-expression-2 number-type `divided `by number-type number-type #`/)
    (unary-expression number-type `increment number-type #`add1)
    (unary-expression number-type `decrement number-type #`sub1)

    (binary-expression number-type `equals number-type boolean-type #`=)
    (binary-expression-2 number-type `less `than number-type boolean-type #`<)
    (binary-expression-2 number-type `greater `than number-type boolean-type #`>)

    (unary-nested-expression-2 number-type `square `root number-type #`sqrt)
    (unary-expression number-type `squared number-type #`sqr)
    (binary-expression-4 number-type `to `the `power `of number-type number-type #`expt)

    (unary-expression number-type `sinus number-type #`sin)
    (unary-expression number-type `cosinus number-type #`cos)
    (unary-expression number-type `tangens number-type #`tan)

    (unary-expression number-type `text text-type #`number->string)

    (binary-expression int-type `plus int-type int-type #`unsafe-fx+/wraparound)
    (binary-expression int-type `minus int-type int-type #`unsafe-fx-/wraparound)
    (binary-expression int-type `times int-type int-type #`unsafe-fx*/wraparound)
    
    (binary-expression int-type `equals int-type boolean-type #`unsafe-fx=)
    (binary-expression-2 int-type `less `than int-type boolean-type #`unsafe-fx<)
    (binary-expression-2 int-type `greater `than int-type boolean-type #`unsafe-fx>)

    (unary-expression int-type `text text-type #`number->string)

    (binary-expression text-type `plus text-type text-type #`string-append)
    (unary-expression text-type `length (field! `length number-type) #`string-length)
    (binary-expression text-type `equals text-type boolean-type #`string=?)))

(define base-tuple base-value-tuple)
