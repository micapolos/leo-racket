#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/type
  leo/compiler/typed)

; TODO: Based on type symbol
(define (type-generate-temporary ($type : Type)) : Syntax
  (car (generate-temporaries `(tmp))))

(define (typed-syntax-generate-temporary ($typed : (Typed Syntax Type))) : Syntax
  (type-generate-temporary (typed-type $typed)))

(define (typed-syntax-stack-generate-temporaries 
  ($typed-syntax-stack : (Stackof (Typed Syntax Type)))) 
  : (Stackof Syntax)
  (map typed-syntax-generate-temporary $typed-syntax-stack))
