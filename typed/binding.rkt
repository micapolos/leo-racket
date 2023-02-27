#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/base
  leo/typed/option
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/typed/syntax-get
  leo/typed/type
  leo/typed/type-syntax
  leo/typed/type-decompile
  leo/typed/types
  leo/typed/typed
  leo/typed/type-match
  leo/typed/syntax-match
  leo/typed/syntax-resolve
  leo/typed/syntax-typed
  leo/typed/testing)

(struct argument-binding (
  (type : Type)
  (syntax : Syntax))
  #:transparent
  #:type-name ArgumentBinding)

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

(define-type Binding (U ArgumentBinding ConstantBinding FunctionBinding))

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
    (types-match?
      (map syntax-type $args)
      (function-binding-param-types $function-binding))
    (syntax-with-type
      (datum->syntax #f 
        `(,(function-binding-identifier $function-binding)
          ,@(filter syntax-is-dynamic? $args)))
      (function-binding-return-type $function-binding))))

(check-equal?
  (option-map
    (function-binding-resolve
      (function-binding `foo (list string-type number-type) boolean-type #`fn)
      `foo
      (list 
        (syntax-with-type #`a string-type)
        (syntax-with-type #`b number-type)))
    syntax-typed-datum)
  (typed `(fn a b) boolean-type))

(check-equal?
  (option-map
    (function-binding-resolve
      (function-binding `foo (list string-type (tuple `foo null) number-type) boolean-type #`fn)
      `foo
      (list 
        (syntax-with-type #`a string-type)
        (syntax-with-type #`b (tuple `foo null))
        (syntax-with-type #`c number-type)))
    syntax-typed-datum)
  (typed `(fn a c) boolean-type))

(check-equal?
  (option-map
    (function-binding-resolve
      (function-binding `foo (list string-type number-type) boolean-type #`fn)
      `foo
      (list 
        (syntax-with-type #`a number-type)
        (syntax-with-type #`b number-type)))
    syntax-typed-datum)
  #f)

(check-equal?
  (option-map
    (function-binding-resolve
      (function-binding `foo (list string-type number-type) boolean-type #`bool)
      `not-foo
      (list 
        (syntax-with-type #`a string-type)
        (syntax-with-type #`b number-type)))
    syntax-typed-datum)
  #f)

; --------------------------------------------------------------------

(define
  (argument-binding-resolve
    ($argument-binding : ArgumentBinding) 
    ($symbol : Symbol)) 
  : (Option Syntax)
  (define $type (argument-binding-type $argument-binding))
  (define $given-type (tuple `given (list $type)))
  (define $syntax (argument-binding-syntax $argument-binding))
  (syntax-get 
    (syntax-with-type $syntax $given-type)
    (tuple $symbol null)))

(check-equal?
  (option-map
    (argument-binding-resolve
      (argument-binding number-type #`tmp)
      `number)
    syntax-typed-datum)
  (typed `tmp number-type))

(check-equal?
  (option-map
    (argument-binding-resolve
      (argument-binding number-type #`tmp)
      `not-number)
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
    (or
      (and
        (constant-binding? (car $binding-list))
        (constant-binding-resolve (car $binding-list) $symbol))
      (and
        (argument-binding? (car $binding-list))
        (argument-binding-resolve (car $binding-list) $symbol))
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
    (or
      (and
        (function-binding? (car $binding-list))
        (function-binding-resolve (car $binding-list) $symbol $args))
      (binding-list-resolve-symbol-args (cdr $binding-list) $symbol $args))))

(check-equal?
  (option-map
    (binding-list-resolve-symbol-args
      (list
        (function-binding `plus (list string-type string-type) string-type #`string-append)
        (constant-binding `bar string-type #`foo-string))
      `plus
      (list
        (syntax-with-type #`a string-type)
        (syntax-with-type #`b string-type)))
    syntax-typed-datum)
  (typed `(string-append a b) string-type))

; --------------------------------------------------------------------

(define
  (binding-list-apply-symbol
    ($binding-list : (Listof Binding))
    ($symbol : Symbol))
  : Syntax
  (or
    (binding-list-resolve-symbol $binding-list $symbol)
    (symbol-make $symbol)))

(check-equal?
  (syntax-typed-datum
    (binding-list-apply-symbol
      (list
        (constant-binding `foo string-type #`foo-string)
        (constant-binding `bar string-type #`foo-string))
      `foo))
  (typed `foo-string string-type))

(check-equal?
  (syntax-typed-datum
    (binding-list-apply-symbol
      (list
        (constant-binding `foo string-type #`foo-string)
        (constant-binding `bar string-type #`foo-string))
      `not-foo))
  (typed #f (tuple `not-foo null)))

; --------------------------------------------------------------------

(define 
  (binding-list-apply-symbol-args
    ($binding-list : (Listof Binding))
    ($symbol : Symbol)
    ($args : (Listof Syntax)))
  : Syntax
  (or 
    (symbol-args-resolve $symbol $args)
    (binding-list-resolve-symbol-args $binding-list $symbol $args)
    (symbol-args-make $symbol $args)))

(check-equal?
  (syntax-typed-datum
    (binding-list-apply-symbol-args
      (list
        (function-binding `plus (list string-type string-type) string-type #`string-append)
        (constant-binding `bar string-type #`foo-string))
      `string
      (list 
        (syntax-with-type #`x 
          (tuple `id (list number-type string-type))))))
  (typed `(unsafe-cdr x) string-type))

(check-equal?
  (syntax-typed-datum
    (binding-list-apply-symbol-args
      (list
        (function-binding `plus (list string-type string-type) string-type #`string-append)
        (constant-binding `bar string-type #`foo-string))
      `plus
      (list
        (syntax-with-type #`a string-type)
        (syntax-with-type #`b string-type))))
  (typed `(string-append a b) string-type))

(check-equal?
  (syntax-typed-datum
    (binding-list-apply-symbol-args
      (list
        (function-binding `plus (list string-type string-type) string-type #`string-append)
        (constant-binding `bar string-type #`foo-string))
      `not-plus
      (list
        (syntax-with-type #`a string-type)
        (syntax-with-type #`b string-type))))
  (typed 
    `(cons a b) 
    (tuple `not-plus (list string-type string-type))))

; --------------------------------------------------------------------

(define types-submod-name `types)

(define (binding-list-type-module-syntax ($binding-list : (Listof Binding))) : Syntax
  (cast-syntax
    (datum->syntax #f
      `(module+ 
        ,types-submod-name
        (provide (all-defined-out))
        (require leo/type-runtime)
        ,@(reverse (binding-list-type-syntaxes $binding-list))))))

(define (binding-list-type-syntaxes ($binding-list : (Listof Binding))) : (Listof Syntax)
  (map binding-type-syntax $binding-list))

(define (binding-type-syntax ($binding : Binding)) : Syntax
  (cond
    ((argument-binding? $binding) #`(void))
    ((constant-binding? $binding) (constant-binding-type-syntax $binding))
    ((function-binding? $binding) (function-binding-type-syntax $binding))))

(define (constant-binding-type-syntax ($constant-binding : ConstantBinding)) : Syntax
  (cast-syntax
    #`(define
      #,(constant-binding-identifier $constant-binding)
      #,(type-syntax (constant-binding-type $constant-binding)))))
     
(define (function-binding-type-syntax ($function-binding : FunctionBinding)) : Syntax
  (cast-syntax 
    #`(define
      #,(function-binding-identifier $function-binding)
      #,(type-syntax
        (arrow 
          (function-binding-param-types $function-binding)
          (function-binding-return-type $function-binding))))))

; (check-equal?
;   (binding-list-type-module-syntax
;     (list
;       (function-binding `plus (list string-type string-type) string-type #`string-append)
;       (constant-binding `bar string-type #`foo-string)))
;   #f)
