#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/option
  leo/typed/type-match
  leo/typed/type)

(define (syntax-with-type ($syntax : Syntax) ($type : Type)) : Syntax
  (syntax-property $syntax `type $type))

(define (syntax-type-option ($syntax : Syntax)) : (Option Type)
  (define $value (syntax-property $syntax `type))
  (and $value (cast $value Type)))

(define (syntax-type ($syntax : Syntax)) : Type
  (or
    (syntax-type-option $syntax)
    (error (format "Not typed ~s" $syntax))))

(define (syntax-as ($syntax : Syntax) ($type : Type)) : Syntax
  (define $syntax-type-option (syntax-type-option $syntax))
  (if 
    (and 
      $syntax-type-option
      (not (type-matches? $syntax-type-option $type)))
    (error (format "(syntax-as ~a ~a)" $syntax $type))
    (void))
  (syntax-with-type $syntax $type))
