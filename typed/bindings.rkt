#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/type
  leo/typed/types
  leo/typed/syntax-type
  leo/typed/syntax-typed)

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
    (syntax->typed $syntax)
    (bindings-parse-complex-syntax $bindings $syntax)))

(define
  (bindings-parse-complex-syntax
    ($bindings : Bindings)
    ($syntax : (Syntaxof Any))) : (U (Syntaxof Any) False)
  #f)

(define
  (bindings-syntax
    ($bindings : Bindings)
    ($syntax : (Syntaxof Any))) : (Syntaxof Any)
  (bindings-resolve $bindings
    (bindings-syntax-inner $bindings $syntax)))

(define
  (bindings-syntax-inner
    ($bindings : Bindings)
    ($syntax : (Syntaxof Any))) : (Syntaxof Any)
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((boolean? $syntax-e)
        (syntax-with-type $syntax boolean-type))
      ((number? $syntax-e)
        (syntax-with-type $syntax number-type))
      ((string? $syntax-e)
        (syntax-with-type $syntax string-type))
      ((symbol? $syntax-e)
        (syntax-with-type #`() (field-type $syntax-e void-type-body)))
      ((and (pair? $syntax-e) (list? $syntax-e))
        (let (($car (car $syntax-e))
              ($cdr (cast (cdr $syntax-e) (Listof (Syntaxof Any)))))
          (cond
            ((identifier? $car) 
              (typed-field-syntax 
                $car 
                (map (curry bindings-syntax-inner $bindings) $cdr)))
            (else (error (format "Identifier expected ~v" $car))))))
      (else (error (format "Parse error ~v" $syntax))))))

; TODO
(define 
  (bindings-resolve 
    ($bindings : Bindings)
    ($syntax : (Syntaxof Any))) : (Syntaxof Any)
  $syntax)

(define
  (bindings-plus-syntax 
    ($bindings : Bindings)
    ($syntax : (Syntaxof Any))) : (Option Bindings)
  #f)
