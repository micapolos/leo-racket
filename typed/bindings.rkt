#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/type
  leo/typed/types
  leo/typed/syntax-match
  leo/typed/syntax-type
  leo/typed/syntax-typed)

(struct binding ((type : Type) (syntax : Syntax))
  #:transparent
  #:type-name Binding)

(struct bindings ((list : (Listof Binding)))
  #:transparent
  #:type-name Bindings)

(define null-bindings (bindings null))

(define 
  (bindings-plus
    ($bindings : Bindings) 
    ($binding : Binding)) : Bindings
  (bindings (cons $binding (bindings-list $bindings))))

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
      ((and (list? $syntax-e) (identifier? (car $syntax-e)))
        (cond 
          ((equal? (syntax-e (car $syntax-e)) `function)
            (error "TODO: function"))
          (else 
            (typed-field-syntax
              (car $syntax-e)
              (map (curry bindings-syntax-inner $bindings) (cdr $syntax-e))))))
      (else (error (format "Parse error ~v" $syntax))))))

; TODO
(define
  (bindings-resolve 
    ($bindings : Bindings)
    ($syntax : Syntax)) : Syntax
  (let 
    (($found 
      (findf 
        (lambda (($binding : Binding))
          (equal? (binding-type $binding) (syntax-type $syntax)))
        (bindings-list $bindings))))
    (or
      (and $found (binding-syntax $found))
      $syntax)))

(define
  (bindings-plus-syntax
    ($bindings : Bindings)
    ($syntax : Syntax)) : (Option Bindings)
  (syntax-match-symbol-rhs $syntax `define 
    (lambda (($define-syntaxes : (Listof Syntax)))
      $bindings)))
