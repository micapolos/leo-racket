#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/syntax-type)

(struct binding ((syntax : (Syntaxof Any)) (type : Type))
  #:transparent
  #:type-name Binding)

(struct bindings ((list : (Listof (Pairof Type Binding))))
  #:transparent
  #:type-name Bindings)

(define null-bindings (bindings null))

(define 
  (bindings-plus
    ($bindings : Bindings) 
    ($type : Type) 
    ($binding : Binding)) : Bindings
  (bindings (cons (cons $type $binding) (bindings-list $bindings))))

(define 
  (bindings-ref
    ($bindings : Bindings) 
    ($type : Type)) : (U Binding False)
  (define $assoc (assoc $type (bindings-list $bindings)))
  (if (equal? $assoc #f) #f (cdr $assoc)))

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
