#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/string
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-symbol
  leo/compiler/typed
  (for-syntax racket/base))

(define tmp-temporaries? : (Parameter Boolean) (make-parameter #f))

(define-syntax (tmp-do $syntax)
  (syntax-case $syntax () 
    ((_ body ...)
      #`(parameterize ((tmp-temporaries? #t))
        body ...))))

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
  (tmp-do (syntax->datum (type-generate-temporary type-a)))
  `tmp-a)

; -------------------------------------------------------------------------

(define (type-generate-temporary-option ($type : Type)) : (Option Identifier)
  (and 
    (type-dynamic? $type)
    (type-generate-temporary $type)))
