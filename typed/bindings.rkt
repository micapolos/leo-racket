#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/type
  leo/typed/typed
  leo/typed/types
  leo/typed/values
  leo/typed/syntax-match
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/typed/type-parse
  leo/testing)

(define-type
  binding-key 
  (U Identifier (Pairof (Listof Identifier) (Listof Syntax))))

(struct binding ((type : Type) (syntax : Syntax) (function? : Boolean))
  #:transparent
  #:type-name Binding)

(struct bindings ((list : (Listof Binding)))
  #:transparent
  #:type-name Bindings)

(define null-bindings (bindings null))

(define pi-binding
  (binding 
    (symbol-type `pi)
    (syntax-with-type #`pi number-type)
    #f))

(define string-length-binding
  (binding 
    (field-type `length (struct-type-body (list string-type)))
    (syntax-with-type
      #`string-length
      (arrow-type
        (list (field-type `length (struct-type-body (list string-type))))
        (list number-type)))
    #t))

(define string-append-binding
  (binding 
    (field-type `plus (struct-type-body (list string-type string-type)))
    (syntax-with-type
      #`string-append
      (arrow-type
        (list (field-type `plus (struct-type-body (list string-type string-type))))
        (list string-type)))
    #t))

(define +-binding
  (binding 
    (field-type `plus (struct-type-body (list number-type number-type)))
    (syntax-with-type
      #`+
      (arrow-type
        (list (field-type `plus (struct-type-body (list number-type number-type))))
        (list number-type)))
    #t))

(define base-bindings (bindings (list 
  pi-binding
  string-append-binding
  string-length-binding
  +-binding)))

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

; -------------------------------------------------------------------

(define 
  (binding-values-syntax
    ($binding : Binding)
    ($values : Values)) : Syntax
  (cond
    ((not (binding-function? $binding))
      (binding-syntax $binding))
    (else
      (let* (($binding-syntax (binding-syntax $binding))
             ($arrow (type-arrow (syntax-type $binding-syntax)))
             ($lhs-types (arrow-type-lhs-types $arrow))
             ($rhs-types (arrow-type-rhs-types $arrow)))
        (if (= (length $rhs-types) 1)
          (syntax-with-type
            (datum->syntax #f 
              (cons $binding-syntax (values-syntaxes $values)))
            (car $rhs-types))
          (error "Arrow with multi-value return type"))))))

(check-equal?
  (syntax-typed-datum
    (binding-values-syntax
      pi-binding
      (values 
        null
        (symbol-type `pi))))
  (typed `pi number-type))

(check-equal?
  (syntax-typed-datum
    (binding-values-syntax
      string-append-binding
      (values 
        (list 
          (syntax-with-type #`"foo" string-type)
          (syntax-with-type #`"bar" string-type))
        (field-type `plus (struct-type-body (list string-type string-type))))))
  (typed `(string-append "foo" "bar") string-type))

; -------------------------------------------------------------------

(define
  (bindings-values-syntax
    ($bindings : Bindings)
    ($values : Values)) : Syntax
  (let 
    (($found 
      (findf
        (lambda (($binding : Binding))
          (equal? (binding-type $binding) (values-type $values)))
        (bindings-list $bindings))))
    (or
      (and $found (binding-values-syntax $found $values))
      (values-syntax $values))))

(check-equal?
  (syntax-typed-datum
    (bindings-values-syntax
      base-bindings
      (values
        (list
          (syntax-with-type #`1 number-type)
          (syntax-with-type #`"foo" string-type))
        (field-type `id (struct-type-body (list number-type string-type))))))
  (typed 
    `(vector-immutable 1 "foo")
    (field-type `id (struct-type-body (list number-type string-type)))))

(check-equal?
  (syntax-typed-datum
    (bindings-values-syntax
      base-bindings
      (values
        (list
          (syntax-with-type #`"foo" number-type)
          (syntax-with-type #`"bar" string-type))
        (field-type `plus (struct-type-body (list string-type string-type))))))
  (typed 
    `(string-append "foo" "bar")
    string-type))

; -------------------------------------------------------------------

(define
  (bindings-syntax
    ($bindings : Bindings)
    ($syntax : Syntax)) : Syntax
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((null? $syntax-e) 
        (error "null syntax???"))
      ((boolean? $syntax-e)
        (syntax-with-type $syntax boolean-type))
      ((number? $syntax-e)
        (syntax-with-type $syntax number-type))
      ((string? $syntax-e)
        (syntax-with-type $syntax string-type))
      (else
        (define $values
          (cond
            ((symbol? $syntax-e)
              (values null (symbol-type $syntax-e)))
            ((and (list? $syntax-e) (identifier? (car $syntax-e)))
              (define $identifier (car $syntax-e))
              (define $symbol (syntax-e $identifier))
              (define $rhs-syntaxes (cdr $syntax-e))
              (cond 
                ((equal? $symbol `function)
                  (error "TODO: function"))
                ((and (equal? $symbol `as) (equal? (length $rhs-syntaxes) 2))
                  (define $as-lhs-syntax (car $rhs-syntaxes))
                  (define $as-rhs-syntax (cadr $rhs-syntaxes))
                  (define $lhs-syntax (bindings-syntax $bindings $as-lhs-syntax))
                  (define $rhs-type (syntax-parse-type $as-rhs-syntax))
                  (values
                    (list (syntax-as $lhs-syntax $rhs-type))
                    $rhs-type))
                (else 
                  (define $resolved-rhs-syntaxes 
                    (map (curry bindings-syntax $bindings) $rhs-syntaxes))
                  (values
                    $resolved-rhs-syntaxes
                    (field-type 
                      $symbol
                      (struct-type-body (map syntax-type $resolved-rhs-syntaxes)))))))
            (else (error (format "values error ~v" $syntax)))))
        (bindings-values-syntax $bindings $values)))))

; TODO: Replace with boolean true / boolean false
(check-equal?
  (syntax-typed-datum (bindings-syntax base-bindings #`#t))
  (typed #t boolean-type))

(check-equal?
  (syntax-typed-datum (bindings-syntax base-bindings #`1))
  (typed 1 number-type))

(check-equal?
  (syntax-typed-datum (bindings-syntax base-bindings #`"foo"))
  (typed "foo" string-type))

(check-equal?
  (syntax-typed-datum (bindings-syntax base-bindings #`foo))
  (typed `() (symbol-type `foo)))

(check-equal?
  (syntax-typed-datum (bindings-syntax base-bindings #`pi))
  (typed `pi number-type))

(check-equal?
  (syntax-typed-datum
    (bindings-syntax
      base-bindings
      #`(plus "foo" "bar")))
  (typed 
    `(string-append "foo" "bar")
    string-type))
