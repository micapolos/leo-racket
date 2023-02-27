#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/option
  leo/typed/base
  leo/typed/type
  leo/typed/types
  leo/typed/type-match
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/typed/typed
  leo/typed/typed-syntax
  leo/typed/testing)

(data binding
  (param-type-list : (Listof Type))
  (return-type : Type)
  (identifier : Identifier)
  (function? : Boolean))

; ----------------------------------------------------------------------------

(define 
  (binding-resolve 
    ($binding : Binding) 
    ($syntax-list : (Listof Syntax))) 
  : (Option Syntax)
  (and
    (types-match?
      (map syntax-type $syntax-list)
      (binding-param-type-list $binding))
    (syntax-with-type
      (datum->syntax #f
        (if (binding-function? $binding)
          `(,(binding-identifier $binding)
            ,@(filter syntax-is-dynamic? $syntax-list))
          (binding-identifier $binding)))
      (binding-return-type $binding))))

(check-equal?
  (option-map
    (binding-resolve 
      (binding (list number-type string-type) boolean-type #`foo #f)
      (list (syntax-with-type #`a number-type) (syntax-with-type #`b string-type)))
    syntax-typed-datum)
  (typed `foo boolean-type))

(check-equal?
  (option-map
    (binding-resolve 
      (binding (list number-type string-type) boolean-type #`foo #t)
      (list (syntax-with-type #`a number-type) (syntax-with-type #`b string-type)))
    syntax-typed-datum)
  (typed `(foo a b) boolean-type))

(check-equal?
  (binding-resolve 
    (binding (list number-type string-type) boolean-type #`foo #f)
    (list (syntax-with-type #`a number-type) (syntax-with-type #`b number-type)))
  #f)

; ----------------------------------------------------------------------------

(define
  (binding-list-resolve
    ($binding-list : (Listof Binding))
    ($syntax-list : (Listof Syntax)))
  : (Option Syntax)
  (and
    (not (null? $binding-list))
    (or
      (binding-resolve (car $binding-list) $syntax-list)
      (binding-list-resolve (cdr $binding-list) $syntax-list))))

(check-equal?
  (option-map
    (binding-list-resolve 
      (list 
        (binding (list number-type) boolean-type #`number->boolean #t)
        (binding (list string-type) boolean-type #`string->boolean #t))
      (list (syntax-with-type #`a number-type)))
    syntax-typed-datum)
  (typed `(number->boolean a) boolean-type))

(check-equal?
  (option-map
    (binding-list-resolve 
      (list 
        (binding (list number-type) boolean-type #`number->boolean #t)
        (binding (list string-type) boolean-type #`string->boolean #t))
      (list (syntax-with-type #`a string-type)))
    syntax-typed-datum)
  (typed `(string->boolean a) boolean-type))

(check-equal?
  (binding-list-resolve 
    (list 
      (binding (list number-type) boolean-type #`number->boolean #t)
      (binding (list string-type) boolean-type #`string->boolean #t))
    (list (syntax-with-type #`a boolean-type)))
  #f)
