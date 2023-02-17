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
    ((= (length $args) 1)
      (syntax-get (car $args) (symbol-type $symbol)))
    (else #f)))

(define 
  (symbol-args-make 
    ($symbol : Symbol) 
    ($args : (Listof Syntax))) : Syntax
  (syntax-with-type
    (let (($dynamic-args (filter syntax-is-dynamic? $args)))
      (cond
        ((null? $dynamic-args) #`())
        ((null? (cdr $dynamic-args)) (car $dynamic-args))
        (else (datum->syntax #f (cons `vector $dynamic-args)))))
    (field-type $symbol (struct-type-body (map syntax-type $args)))))
