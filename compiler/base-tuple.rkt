#lang typed/racket/base

(provide (all-defined-out))

(require
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
    (symbol-expression `boolean type-type #`boolean-type)
    (symbol-expression `number type-type #`number-type)
    (symbol-expression `int type-type #`int-type)
    (symbol-expression `float type-type #`float-type)
    (symbol-expression `text type-type #`text-type)))

(define base-value-tuple
  (tuple
    (binary-expression number-type `add number-type number-type #`+)
    (binary-expression number-type `subtract number-type number-type #`-)
    (binary-expression-2 number-type `multiply `by number-type number-type #`*)
    (binary-expression-2 number-type `divide `by number-type number-type #`/)
    (unary-expression number-type `increment number-type #`add1)
    (unary-expression number-type `decrement number-type #`sub1)

    (binary-expression-2 number-type `check `equals number-type check-type #`=)
    (binary-expression-3 number-type `check `less `than number-type check-type #`<)
    (binary-expression-3 number-type `check `greater `than number-type check-type #`>)

    (unary-nested-expression-3 number-type `get `square `root number-type #`sqrt)
    (unary-nested-expression-2 number-type `get `square number-type #`sqr)

    (unary-nested-expression-2 number-type `get `sinus number-type #`sin)
    (unary-nested-expression-2 number-type `get `cosinus number-type #`cos)
    (unary-nested-expression-2 number-type `get `tangens number-type #`tan)

    (unary-nested-expression-2 number-type `get `text text-type #`number->string)

    (binary-expression text-type `append text-type text-type #`string-append)
    (unary-nested-expression-2 text-type `get `length (field! `length number-type) #`string-length)
    (binary-expression-2 text-type `check `equals text-type check-type #`string=?)

    ; deprecated
    (binary-expression number-type `plus number-type number-type #`+)
    (binary-expression number-type `minus number-type number-type #`-)
    (binary-expression number-type `times number-type number-type #`*)
    (binary-expression-2 number-type `divided `by number-type number-type #`/)
    (unary-expression number-type `increment number-type #`add1)
    (unary-expression number-type `decrement number-type #`sub1)

    (binary-expression number-type `equals number-type boolean-type #`=)
    (binary-expression-2 number-type `less `than number-type boolean-type #`<)
    (binary-expression-2 number-type `greater `than number-type boolean-type #`>)

    (unary-expression-2 number-type `square `root number-type #`sqrt)
    (unary-expression-2 number-type `squared `root number-type #`sqr)

    (unary-expression number-type `sinus number-type #`sin)
    (unary-expression number-type `cosinus number-type #`cos)
    (unary-expression number-type `tangens number-type #`tan)

    (unary-expression number-type `text text-type #`number->string)

    (binary-expression int-type `plus int-type int-type #`unsafe-fx/wraparound+)
    (binary-expression int-type `minus int-type int-type #`unsafe-fx/wraparound-)
    (binary-expression int-type `times int-type int-type #`unsafe-fx/wraparound*)
    
    (binary-expression int-type `equals int-type boolean-type #`unsafe-fx=)
    (binary-expression-2 int-type `less `than int-type boolean-type #`unsafe-fx<)
    (binary-expression-2 int-type `greater `than int-type boolean-type #`unsafe-fx>)

    (unary-expression int-type `text text-type #`number->string)

    (binary-expression text-type `plus text-type text-type #`string-append)
    (unary-expression-2 text-type `length `number number-type #`string-length)
    (unary-expression-2 text-type `length `int int-type #`string-length)
    (binary-expression text-type `equals text-type boolean-type #`string=?)))

(define base-tuple base-value-tuple)
