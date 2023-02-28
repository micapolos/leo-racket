#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/string
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-symbol
  leo/compiler/typed)

(define (type-generate-temporary ($type : Type)) : Identifier
  (car (generate-temporaries (list (type-symbol $type)))))

(check-equal?
  (string-prefix?
    (symbol->string (syntax-e (type-generate-temporary type-a)))
    (symbol->string (type-symbol type-a)))
  #t)

; -------------------------------------------------------------------------

(define (typed-syntax-generate-temporary ($typed : (Typed Syntax Type))) : Identifier
  (type-generate-temporary (typed-type $typed)))

(define (typed-syntax-stack-generate-temporaries 
  ($typed-syntax-stack : (Stackof (Typed Syntax Type)))) 
  : (Stackof Identifier)
  (map typed-syntax-generate-temporary $typed-syntax-stack))

