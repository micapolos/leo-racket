#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/type
  leo/typed/typed
  leo/typed/types
  leo/typed/args
  leo/typed/syntax-get
  leo/typed/syntax-match
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/typed/type-parse
  leo/testing)

(define 
  (symbol-make 
    ($symbol : Symbol)) : Syntax
  (syntax-with-type #`() (symbol-type $symbol)))

(define 
  (symbol-args-resolve 
    ($symbol : Symbol) 
    ($args : (Listof Syntax))) : (Option Syntax)
  (cond
    ((and (equal? $symbol `get) (= (length $args) 2))
      (define $lhs-syntax (car $args))
      (define $rhs-syntax (cadr $args))
      (syntax-get $lhs-syntax (syntax-type $rhs-syntax)))
    (else #f)))

(define 
  (symbol-args-make 
    ($symbol : Symbol) 
    ($args : (Listof Syntax))) : Syntax
  (syntax-with-type
    (datum->syntax #f (cons $symbol (filter syntax-is-dynamic? $args)))
    (field-type $symbol (struct-type-body (map syntax-type $args)))))
