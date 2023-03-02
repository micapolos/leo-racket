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

(define tmp-temporaries? (make-parameter (ann #f Boolean)))

(define (type-generate-temporary ($type : Type)) : Identifier
  (define $symbol (type-symbol $type))
  (cond
    ((tmp-temporaries?)
      (datum->syntax #f 
        (string->symbol (string-append "tmp-" (symbol->string $symbol)))))
    (else (car (generate-temporaries (list (type-symbol $type)))))))

(check-equal?
  (string-prefix?
    (symbol->string (syntax-e (type-generate-temporary type-a)))
    (symbol->string (type-symbol type-a)))
  #t)

(check-equal?
  (parameterize ((tmp-temporaries? #t))
    (syntax->datum (type-generate-temporary type-a)))
  `tmp-a)

; -------------------------------------------------------------------------

(define (type-generate-temporary-option ($type : Type)) : (Option Identifier)
  (and 
    (type-is-dynamic? $type)
    (type-generate-temporary $type)))

; -------------------------------------------------------------------------

(define (typed-syntax-generate-temporary ($typed : (Typed Syntax Type))) : Identifier
  (type-generate-temporary (typed-type $typed)))

(define (typed-syntax-stack-generate-temporaries 
  ($typed-syntax-stack : (Stackof (Typed Syntax Type)))) 
  : (Stackof Identifier)
  (map typed-syntax-generate-temporary $typed-syntax-stack))
