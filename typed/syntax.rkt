#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/testing
  leo/typed/type)

(struct (V) typed (($value : V) ($type : Type))
  #:transparent
  #:type-name Typed)

(define (syntax-typed ($syntax : (Syntaxof Any)) ($type : Type))
  (syntax-property $syntax `type $type))

(define (syntax-type ($syntax : (Syntaxof Any))) : Type
  (define $value (syntax-property $syntax `type))
  (cond
    ((equal? $value #f) (error (format "Non-typed ~s" $syntax)))
    (else (cast $value Type))))

(define (syntax-typed-datum ($syntax : (Syntaxof Any)))
  (typed (syntax->datum $syntax) (syntax-type $syntax)))

(check-equal?
  (syntax->datum (syntax-typed #`1 number-type)) 
  1)

(check-equal?
  (syntax-type (syntax-typed #`1 number-type)) 
  number-type)

(define (syntax-parse ($syntax : (Syntaxof Any))) : (Option (Syntaxof Any))
  (let (($datum (syntax-e $syntax)))
    (cond
      ((boolean? $datum) 
        (syntax-typed $syntax boolean-type))
      ((number? $datum) 
        (syntax-typed $syntax number-type))
      ((string? $datum)
        (syntax-typed $syntax string-type))
      (else #f))))

(check-equal?
  (syntax-typed-datum (option-ref (syntax-parse #`1))) 
  (typed 1 number-type))

(check-equal?
  (syntax-typed-datum (option-ref (syntax-parse #`"foo"))) 
  (typed "foo" string-type))

(check-equal? 
  (syntax-typed-datum (option-ref (syntax-parse #`#f)))
  (typed #f boolean-type))

(check-equal? (syntax-parse #`foo) #f)
