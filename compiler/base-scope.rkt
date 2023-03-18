#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/scope
  leo/compiler/binding
  leo/compiler/binding-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define assembly-scope
  (scope
    (unary-binding boolean-type `disassemble (choice! (field! `true) (field! `false)) #`identity)
    (binary-binding (choice! (field! `true) (field! `false)) `assemble (field! `boolean) boolean-type #`identity)))

(define base-type-scope
  (scope
    (symbol-binding `boolean type-type #`boolean-type)
    (symbol-binding `number type-type #`number-type)
    (symbol-binding `int type-type #`int-type)
    (symbol-binding `float type-type #`float-type)
    (symbol-binding `text type-type #`text-type)))

(define base-value-scope
  (scope
    (binary-binding number-type `add number-type number-type #`+)
    (binary-binding number-type `subtract number-type number-type #`-)
    (binary-binding-2 number-type `multiply `by number-type number-type #`*)
    (binary-binding-2 number-type `divide `by number-type number-type #`/)
    (unary-binding number-type `increment number-type #`add1)
    (unary-binding number-type `decrement number-type #`sub1)

    (binary-binding-2 number-type `check `equals number-type check-type #`=)
    (binary-binding-3 number-type `check `less `than number-type check-type #`<)
    (binary-binding-3 number-type `check `greater `than number-type check-type #`>)

    (unary-nested-binding-3 number-type `get `square `root number-type #`sqrt)
    (unary-nested-binding-2 number-type `get `square number-type #`sqr)

    (unary-nested-binding-2 number-type `get `sinus number-type #`sin)
    (unary-nested-binding-2 number-type `get `cosinus number-type #`cos)
    (unary-nested-binding-2 number-type `get `tangens number-type #`tan)

    (unary-nested-binding-2 number-type `get `text text-type #`number->string)

    (binary-binding text-type `append text-type text-type #`string-append)
    (unary-nested-binding-2 text-type `get `length (field! `length number-type) #`string-length)
    (binary-binding-2 text-type `check `equals text-type check-type #`string=?)

    ; deprecated
    (binary-binding number-type `plus number-type number-type #`+)
    (binary-binding number-type `minus number-type number-type #`-)
    (binary-binding number-type `times number-type number-type #`*)
    (binary-binding-2 number-type `divided `by number-type number-type #`/)
    (unary-binding number-type `increment number-type #`add1)
    (unary-binding number-type `decrement number-type #`sub1)

    (binary-binding number-type `equals number-type boolean-type #`=)
    (binary-binding-2 number-type `less `than number-type boolean-type #`<)
    (binary-binding-2 number-type `greater `than number-type boolean-type #`>)

    (unary-binding-2 number-type `square `root number-type #`sqrt)
    (unary-binding-2 number-type `squared `root number-type #`sqr)

    (unary-binding number-type `sinus number-type #`sin)
    (unary-binding number-type `cosinus number-type #`cos)
    (unary-binding number-type `tangens number-type #`tan)

    (unary-binding number-type `text text-type #`number->string)

    (binary-binding int-type `plus int-type int-type #`unsafe-fx/wraparound+)
    (binary-binding int-type `minus int-type int-type #`unsafe-fx/wraparound-)
    (binary-binding int-type `times int-type int-type #`unsafe-fx/wraparound*)
    
    (binary-binding int-type `equals int-type boolean-type #`unsafe-fx=)
    (binary-binding-2 int-type `less `than int-type boolean-type #`unsafe-fx<)
    (binary-binding-2 int-type `greater `than int-type boolean-type #`unsafe-fx>)

    (unary-binding int-type `text text-type #`number->string)

    (binary-binding text-type `plus text-type text-type #`string-append)
    (unary-binding-2 text-type `length `number number-type #`string-length)
    (unary-binding-2 text-type `length `int int-type #`string-length)
    (binary-binding text-type `equals text-type boolean-type #`string=?)))

(define base-scope base-value-scope)
