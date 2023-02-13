#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/option
  leo/typed/type)

(define (syntax-with-type ($syntax : Syntax) ($type : Type))
  (syntax-property $syntax `type $type))

(define (syntax-type ($syntax : Syntax)) : Type
  (define $value (syntax-property $syntax `type))
  (cond
    ((equal? $value #f) (error (format "Not typed ~s" $syntax)))
    (else (cast $value Type))))
