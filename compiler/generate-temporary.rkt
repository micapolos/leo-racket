#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/string
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-symbol
  leo/compiler/typed
  (for-syntax racket/base))

(define (symbol-temporary ($symbol : Symbol)) : Identifier
  (cond
    ((testing?)
      (datum->syntax #f 
        (string->symbol (string-append "tmp-" (symbol->string $symbol)))))
    (else (car (generate-temporaries (list $symbol))))))

(define (type-generate-temporary ($type : Type)) : Identifier
  (symbol-temporary (type-symbol $type)))

(check-equal?
  (string-prefix?
    (symbol->string (syntax-e (parameterize ((testing? #f)) (type-generate-temporary type-a))))
    (symbol->string (type-symbol type-a)))
  #t)

(check-equal?
  (syntax->datum (type-generate-temporary type-a))
  `tmp-a)

; -------------------------------------------------------------------------

(define (type-generate-temporary-option ($type : Type)) : (Option Identifier)
  (and 
    (type-dynamic? $type)
    (type-generate-temporary $type)))

; -------------------------------------------------------------------------

(define (structure-temporary-stack ($structure : Structure)) : (Stackof Identifier)
  (filter-false (map type-generate-temporary-option $structure)))
