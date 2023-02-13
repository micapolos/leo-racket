#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/type
  leo/typed/types
  leo/typed/syntax-match
  leo/typed/syntax-type
  leo/typed/syntax-typed)

(struct binding ((syntax : Syntax) (type : Type))
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
    ($syntax : Syntax)) : (Option Syntax)
  (or
    (syntax->typed $syntax)
    (bindings-parse-complex-syntax $bindings $syntax)))

(define
  (bindings-parse-complex-syntax
    ($bindings : Bindings)
    ($syntax : Syntax)) : (Option Syntax)
  #f)

(define
  (bindings-syntax
    ($bindings : Bindings)
    ($syntax : Syntax)) : Syntax
  (bindings-resolve $bindings
    (bindings-syntax-inner $bindings $syntax)))

(define
  (bindings-syntax-inner
    ($bindings : Bindings)
    ($syntax : Syntax)) : Syntax
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((null? $syntax-e) 
        (error "empty syntax"))
      ((boolean? $syntax-e)
        (syntax-with-type $syntax boolean-type))
      ((number? $syntax-e)
        (syntax-with-type $syntax number-type))
      ((string? $syntax-e)
        (syntax-with-type $syntax string-type))
      ((symbol? $syntax-e)
        (syntax-with-type #`() (field-type $syntax-e void-type-body)))
      ((list? $syntax-e)
        (let (($car (car $syntax-e))
              ($cdr (cdr $syntax-e)))
          (cond
            ((identifier? $car)
              (let (($symbol (syntax-e $car)))
                (cond
                  ((equal? $symbol `function)
                    (error "TODO: function"))
                  (else 
                    (typed-field-syntax 
                      $car 
                      (map (curry bindings-syntax-inner $bindings) $cdr))))))
            (else (error (format "Identifier expected ~v" $car))))))
      (else (error (format "Parse error ~v" $syntax))))))

; TODO
(define 
  (bindings-resolve 
    ($bindings : Bindings)
    ($syntax : Syntax)) : Syntax
  $syntax)

(define
  (bindings-plus-syntax
    ($bindings : Bindings)
    ($syntax : Syntax)) : (Option Bindings)
  (syntax-match-symbol-rhs $syntax `define 
    (lambda (($define-syntaxes : (Listof Syntax)))
      $bindings)))
