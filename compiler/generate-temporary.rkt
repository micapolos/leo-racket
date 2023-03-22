#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/string
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-symbol
  leo/compiler/typed
  leo/compiler/syntax-utils
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

(define (tmp-syntax-a) (type-generate-temporary dynamic-type-a))
(define (tmp-syntax-b) (type-generate-temporary dynamic-type-b))
(define (tmp-syntax-c) (type-generate-temporary dynamic-type-c))
(define (tmp-syntax-d) (type-generate-temporary dynamic-type-d))

; -------------------------------------------------------------------------

(define (expression-generate-temporary-option ($expression : Expression)) : (Option Identifier)
  (and
    (type-dynamic? (expression-type $expression))
    (type-generate-temporary (expression-type $expression))))

(define (type-generate-temporary-option ($type : Type)) : (Option Identifier)
  (and 
    (type-dynamic? $type)
    (type-generate-temporary $type)))

(define (type-generate-expression ($type : Type)) : Expression
  (define $tmp-option (type-generate-temporary-option $type))
  (expression (or $tmp-option null-syntax) $type))

(define (structure-generate-tuple ($structure : Structure)) : Tuple
  (map type-generate-expression $structure))

; -------------------------------------------------------------------------

(define (structure-temporary-stack ($structure : Structure)) : (Stackof Identifier)
  (filter-false (map type-generate-temporary-option $structure)))
