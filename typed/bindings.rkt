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
  (binding-args-syntax
    ($binding : Binding)
    ($args : Args)) : Syntax
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
              (cons $binding-syntax (args-syntaxes $args)))
            (car $rhs-types))
          (error "Arrow with multi-value return type"))))))

(define 
  (binding-syntaxes-resolve
    ($binding : Binding)
    ($syntaxes : (Listof Syntax))) : Syntax
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
              (cons $binding-syntax (filter syntax-is-dynamic? $syntaxes)))
            (car $rhs-types))
          (error "Arrow with multi-value return type"))))))

(check-equal?
  (syntax-typed-datum
    (binding-args-syntax
      pi-binding
      (args 
        null
        (symbol-type `pi))))
  (typed `pi number-type))

(check-equal?
  (syntax-typed-datum
    (binding-args-syntax
      string-append-binding
      (args 
        (list 
          (syntax-with-type #`"foo" string-type)
          (syntax-with-type #`"bar" string-type))
        (field-type `plus (struct-type-body (list string-type string-type))))))
  (typed `(string-append "foo" "bar") string-type))

; -------------------------------------------------------------------

(define
  (bindings-args-syntax
    ($bindings : Bindings)
    ($args : Args)) : Syntax
  (let 
    (($found 
      (findf
        (lambda (($binding : Binding))
          (equal? (binding-type $binding) (args-type $args)))
        (bindings-list $bindings))))
    (or
      (and $found (binding-args-syntax $found $args))
      (args-syntax $args))))

(define
  (bindings-symbol-syntaxes-resolve
    ($bindings : Bindings)
    ($symbol : Symbol)
    ($syntaxes : (Listof Syntax))) : (Option Syntax)
  (define $type
    (field-type $symbol 
      (struct-type-body (map syntax-type $syntaxes))))
  (let 
    (($found 
      (findf
        (lambda (($binding : Binding))
          (equal? (binding-type $binding) $type))
        (bindings-list $bindings))))
    (and $found (binding-syntaxes-resolve $found $syntaxes))))

(check-equal?
  (syntax-typed-datum
    (bindings-args-syntax
      base-bindings
      (args
        (list
          (syntax-with-type #`1 number-type)
          (syntax-with-type #`"foo" string-type))
        (field-type `id (struct-type-body (list number-type string-type))))))
  (typed 
    `(vector 1 "foo")
    (field-type `id (struct-type-body (list number-type string-type)))))

(check-equal?
  (syntax-typed-datum
    (bindings-args-syntax
      base-bindings
      (args
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
        (define $args
          (cond
            ((symbol? $syntax-e)
              (args null (symbol-type $syntax-e)))
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
                  (define $lhs-syntax 
                    (if (syntax-symbol-arg? $as-lhs-syntax `native)
                      (cadr (syntax-e $as-lhs-syntax))
                      (bindings-syntax $bindings $as-lhs-syntax)))
                  (define $rhs-type (syntax-parse-type $as-rhs-syntax))
                  (args
                    (list (syntax-as $lhs-syntax $rhs-type))
                    $rhs-type))
                (else
                  (define $resolved-rhs-syntaxes 
                    (map (curry bindings-syntax $bindings) $rhs-syntaxes))
                  (args
                    $resolved-rhs-syntaxes
                    (field-type 
                      $symbol
                      (struct-type-body (map syntax-type $resolved-rhs-syntaxes)))))))
            (else (error (format "args error ~v" $syntax)))))
        (bindings-args-syntax $bindings $args)))))

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

(define 
  (symbol-syntaxes-resolve 
    ($symbol : Symbol) 
    ($syntaxes : (Listof Syntax))) : (Option Syntax)
  (cond
    ((and (equal? $symbol `get) (= (length $syntaxes) 2))
      (define $lhs-syntax (car $syntaxes))
      (define $rhs-syntax (cadr $syntaxes))
      (syntax-get $lhs-syntax (syntax-type $rhs-syntax)))
    (else #f)))

(define 
  ($symbol-syntaxes-resolve 
    ($symbol : Symbol) 
    ($syntaxes : (Listof Syntax))) : (Option Syntax)
  (cond
    ((and (equal? $symbol `get) (= (length $syntaxes) 2))
      (define $lhs-syntax (car $syntaxes))
      (define $rhs-syntax (cadr $syntaxes))
      (syntax-get $lhs-syntax (syntax-type $rhs-syntax)))
    (else #f)))

(define 
  (symbol-syntaxes-make 
    ($symbol : Symbol) 
    ($syntaxes : (Listof Syntax))) : Syntax
  (syntax-with-type
    (datum->syntax #f (cons $symbol $syntaxes))
    (field-type $symbol (struct-type-body (map syntax-type $syntaxes)))))
