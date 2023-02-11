#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/syntax-type)

(define-type Value Any)

(struct bindings ((list : (Listof (Pairof Type Value))))
  #:transparent
  #:type-name Bindings)

(define 
  (bindings-plus
    ($bindings : Bindings) 
    ($type : Type) 
    ($value : Value)) : Bindings
  (bindings (cons (cons $type $value) (bindings-list $bindings))))

(define 
  (bindings-value
    ($bindings : Bindings) 
    ($type : Type)) : (U Value False)
  (assoc $type (bindings-list $bindings)))

(define
  (bindings-parse-syntax
    ($bindings : Bindings)
    ($syntax : (Syntaxof Any))) : (U (Syntaxof Any) False)
  (let (($datum (syntax-e $syntax)))
    (cond
      ((boolean? $datum) 
        (syntax-typed $syntax boolean-type))
      ((number? $datum) 
        (syntax-typed $syntax number-type))
      ((string? $datum) 
        (syntax-typed $syntax string-type))
      (else #f))))

(define
  (bindings-parse-define-syntax 
    ($bindings : Bindings)
    ($syntax : (Syntaxof Any))) : (U Bindings False)
  $bindings)
