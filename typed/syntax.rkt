#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type)

(define (syntax-typed ($syntax : (Syntaxof Any)) ($type : Type))
  (syntax-property $syntax `type $type))

(define (syntax-type ($syntax : (Syntaxof Any))) : (U Type False)
  (define $value (syntax-property $syntax `type))
  (cond
    ((equal? $value #f) #f)
    (else (cast $value Type))))

(define (syntax-parse ($syntax : (Syntaxof Any))) : (U (Syntaxof Any) False)
  (let (($datum (syntax-e $syntax)))
    (cond
      ((boolean? $datum) 
        (syntax-typed $syntax boolean-type))
      ((number? $datum) 
        (syntax-typed $syntax number-type))
      ((string? $datum) 
        (syntax-typed $syntax string-type))
      (else #f))))
