#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  racket/function
  leo/typed/syntax)

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
  (or
    (syntax-parse $syntax)
    (bindings-parse-complex-syntax $bindings $syntax)))

(define
  (bindings-parse-complex-syntax
    ($bindings : Bindings)
    ($syntax : (Syntaxof Any))) : (U (Syntaxof Any) False)
  #f)

(define
  (bindings-parse
    ($bindings : Bindings)
    ($syntax : (Syntaxof Any))) : (U Bindings False)
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((pair? $syntax-e) 
        (let (($car-stx (car $syntax-e))
              ($cdr-stx (cdr $syntax-e)))
          (cond
            ((identifier? $car-stx) #f)
            (else #f))))
      (else #f))))

(define
  (bindings-parse-identifier-syntaxes
    ($bindings : Bindings)
    ($identifier : Identifier)
    ($syntaxes : (Listof (Syntaxof Any)))) : (Syntaxof Any)
  (let (($parsed-syntaxes (map (curry bindings-parse $bindings) $syntaxes)))
    (syntax-typed
      #`(#,$identifier #,@$parsed-syntaxes)
      (struct-type 
        (list 
          (field-type-line 
            (syntax-e $identifier) 
            $parsed-syntaxes))))))
