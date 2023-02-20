#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/type
  leo/typed/typed
  leo/typed/types
  leo/typed/syntax-get
  leo/typed/syntax-match
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/typed/type-parse
  leo/typed/testing)

(define 
  (symbol-make 
    ($symbol : Symbol)) : Syntax
  (syntax-with-type #`() (symbol-type $symbol)))

(define
  (symbol-args-resolve 
    ($symbol : Symbol) 
    ($args : (Listof Syntax))) : (Option Syntax)
  (cond
    ((= (length $args) 1)
      (define $arg (car $args))
      (syntax-get $arg (symbol-type $symbol)))
    ((and (equal? $symbol `get) (= (length $args) 2))
      (syntax-get (car $args) (syntax-type (cadr $args))))
    ((and (equal? $symbol `check-equal?) (= (length $args) 2))
      (syntax-with-type
        (datum->syntax #f `(,$symbol ,@$args))
        void-type))
    (else 
      (symbol-args-resolve-apply $symbol $args))))

(define 
  (symbol-args-make 
    ($symbol : Symbol) 
    ($args : (Listof Syntax))) : Syntax
  (typed-field-syntax $symbol $args))

(define
  (symbol-args-resolve-apply
    ($symbol : Symbol)
    ($args : (Listof Syntax)))
  : (Option Syntax)
  (cond
    ((equal? $symbol `apply) 
      (error "TODO: function application"))
    (else #f)))

