#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/typed/type
  leo/typed/types
  leo/typed/syntax-resolve
  leo/testing)

(struct constant-binding (
  (symbol : Symbol)
  (type : Type)
  (identifier : Identifier))
  #:transparent
  #:type-name ConstantBinding)

(struct function-binding (
  (symbol : Symbol) 
  (param-types : Type) 
  (return-type : Type) 
  (identifier : Identifier))
  #:transparent
  #:type-name FunctionBinding)

(define-type Binding (U ConstantBinding FunctionBinding))

(define
  (constant-binding-resolve
    ($constant-binding : ConstantBinding) 
    ($symbol : Symbol)) 
  : (Option Syntax)
  (and
    (equal? $symbol (constant-binding-symbol $constant-binding))
    (syntax-with-type
      (constant-binding-identifier $constant-binding)
      (constant-binding-type $constant-binding))))

(define 
  (function-binding-resolve
    ($function-binding : FunctionBinding) 
    ($symbol : Symbol)
    ($args : (Listof Syntax)))
  : (Option Syntax)
  (and
    (equal? 
      $symbol 
      (function-binding-symbol $function-binding))
    (equal? 
      (function-binding-param-types $function-binding) 
      (map syntax-type $args))
    (syntax-with-type
      (datum->syntax #f 
        (cons
          (function-binding-identifier $function-binding)
          (map syntax-is-dynamic? $args)))
      (function-binding-return-type $function-binding))))

(define
  (binding-list-resolve-symbol
    ($binding-list : (Listof Binding))
    ($symbol : Symbol))
  : (Option Syntax)
  (and
    (not (null? $binding-list))
    (constant-binding? (car $binding-list))
    (constant-binding-resolve (car $binding-list) $symbol)
    (binding-list-resolve-symbol (cdr $binding-list) $symbol)))

(define
  (binding-list-resolve-symbol-args
    ($binding-list : (Listof Binding))
    ($symbol : Symbol)
    ($args : (Listof Syntax)))
  : (Option Syntax)
  (and
    (not (null? $binding-list))
    (function-binding? (car $binding-list))
    (function-binding-resolve (car $binding-list) $symbol $args)
    (binding-list-resolve-symbol-args (cdr $binding-list) $symbol $args)))

(define
  (binding-list-resolve
    ($binding-list : (Listof Binding))
    ($syntax : Syntax))
  : Syntax
  (let (($e (syntax-e $syntax)))
    (cond
      ((null? $e) 
        (error "null syntax"))
      ((boolean? $e)
        (syntax-with-type $syntax boolean-type))
      ((number? $e)
        (syntax-with-type $syntax number-type))
      ((string? $e)
        (syntax-with-type $syntax string-type))
      ((symbol? $e)
        (define $symbol $e)
        (or
          (binding-list-resolve-symbol $binding-list $symbol)
          (symbol-make $symbol)))
      ((and (list? $e) (identifier? (car $e)))
        (define $symbol (syntax-e (car $e)))
        (define $args (cdr $e))
        (or 
          (symbol-args-resolve $symbol $args)
          (binding-list-resolve-symbol-args $binding-list $symbol $args)
          (symbol-args-make $symbol $args)))
      (else (error (format "Invalid syntax: ~a" $syntax))))))
