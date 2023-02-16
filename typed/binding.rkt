#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/option
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/typed/type
  leo/typed/types
  leo/typed/typed
  leo/typed/syntax-resolve
  leo/typed/syntax-typed
  leo/testing)

(struct constant-binding (
  (symbol : Symbol)
  (type : Type)
  (identifier : Identifier))
  #:transparent
  #:type-name ConstantBinding)

(struct function-binding (
  (symbol : Symbol) 
  (param-types : (Listof Type))
  (return-type : Type) 
  (identifier : Identifier))
  #:transparent
  #:type-name FunctionBinding)

(define-type Binding (U ConstantBinding FunctionBinding))

; -------------------------------------------------------------

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

(check-equal?
  (option-map
    (constant-binding-resolve
      (constant-binding `foo string-type #`foo-string)
      `foo)
    syntax-typed-datum)
  (typed `foo-string string-type))

(check-equal?
  (option-map
    (constant-binding-resolve
      (constant-binding `foo string-type #`foo-string)
      `not-foo)
    syntax-typed-datum)
  #f)

; --------------------------------------------------------------

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
          (filter syntax-is-dynamic? $args)))
      (function-binding-return-type $function-binding))))

(check-equal?
  (option-map
    (function-binding-resolve
      (function-binding `foo (list string-type number-type) boolean-type #`bool)
      `foo
      (list 
        (syntax-with-type #`"a" string-type)
        (syntax-with-type #`1 number-type)))
    syntax-typed-datum)
  (typed `(bool "a" 1) boolean-type))

(check-equal?
  (option-map
    (function-binding-resolve
      (function-binding `foo (list string-type (symbol-type `empty) number-type) boolean-type #`bool)
      `foo
      (list 
        (syntax-with-type #`"a" string-type)
        (syntax-with-type #`() (symbol-type `empty))
        (syntax-with-type #`1 number-type)))
    syntax-typed-datum)
  (typed `(bool "a" 1) boolean-type))

(check-equal?
  (option-map
    (function-binding-resolve
      (function-binding `foo (list string-type number-type) boolean-type #`bool)
      `foo
      (list 
        (syntax-with-type #`1 number-type)
        (syntax-with-type #`1 number-type)))
    syntax-typed-datum)
  #f)

(check-equal?
  (option-map
    (function-binding-resolve
      (function-binding `foo (list string-type number-type) boolean-type #`bool)
      `not-foo
      (list 
        (syntax-with-type #`"a" string-type)
        (syntax-with-type #`1 number-type)))
    syntax-typed-datum)
  #f)

; --------------------------------------------------------------------

(define
  (binding-list-resolve-symbol
    ($binding-list : (Listof Binding))
    ($symbol : Symbol))
  : (Option Syntax)
  (and
    (not (null? $binding-list))
    (constant-binding? (car $binding-list))
    (or 
      (constant-binding-resolve (car $binding-list) $symbol)
      (binding-list-resolve-symbol (cdr $binding-list) $symbol))))

(check-equal?
  (option-map
    (binding-list-resolve-symbol
      (list
        (constant-binding `foo string-type #`foo-string)
        (constant-binding `bar string-type #`foo-string))
      `foo)
    syntax-typed-datum)
  (typed `foo-string string-type))

; --------------------------------------------------------------------

(define
  (binding-list-resolve-symbol-args
    ($binding-list : (Listof Binding))
    ($symbol : Symbol)
    ($args : (Listof Syntax)))
  : (Option Syntax)
  (and
    (not (null? $binding-list))
    (function-binding? (car $binding-list))
    (or 
      (function-binding-resolve (car $binding-list) $symbol $args)
      (binding-list-resolve-symbol-args (cdr $binding-list) $symbol $args))))

; --------------------------------------------------------------------

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

