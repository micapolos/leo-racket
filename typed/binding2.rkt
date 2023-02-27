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

(data binding2
  (param-type-list : (Listof Type))
  (return-type : Type)
  (identifier : Identifier)
  (function? : Boolean))

; ----------------------------------------------------------------------------

(define 
  (binding2-resolve 
    ($binding : Binding2) 
    ($syntax-list : (Listof Syntax))) 
  : (Option Syntax)
  (and
    (types-match?
      (map syntax-type $syntax-list)
      (binding2-param-type-list $binding))
    (syntax-with-type
      (datum->syntax #f
        (if (binding2-function? $binding)
          `(,(binding2-identifier $binding)
            ,@(filter syntax-is-dynamic? $syntax-list))
          (binding2-identifier $binding)))
      (binding2-return-type $binding))))

(check-equal?
  (option-map
    (binding2-resolve 
      (binding2 (list number-type string-type) boolean-type #`foo #f)
      (list (syntax-with-type #`a number-type) (syntax-with-type #`b string-type)))
    syntax-typed-datum)
  (typed `foo boolean-type))

(check-equal?
  (option-map
    (binding2-resolve 
      (binding2 (list number-type string-type) boolean-type #`foo #t)
      (list (syntax-with-type #`a number-type) (syntax-with-type #`b string-type)))
    syntax-typed-datum)
  (typed `(foo a b) boolean-type))

(check-equal?
  (binding2-resolve 
    (binding2 (list number-type string-type) boolean-type #`foo #f)
    (list (syntax-with-type #`a number-type) (syntax-with-type #`b number-type)))
  #f)

; ----------------------------------------------------------------------------

(define
  (binding2-list-resolve
    ($binding-list : (Listof Binding2))
    ($syntax-list : (Listof Syntax)))
  : (Option Syntax)
  (and
    (not (null? $binding-list))
    (or
      (binding2-resolve (car $binding-list) $syntax-list)
      (binding2-list-resolve (cdr $binding-list) $syntax-list))))

(check-equal?
  (option-map
    (binding2-list-resolve 
      (list 
        (binding2 (list number-type) boolean-type #`number->boolean #t)
        (binding2 (list string-type) boolean-type #`string->boolean #t))
      (list (syntax-with-type #`a number-type)))
    syntax-typed-datum)
  (typed `(number->boolean a) boolean-type))

(check-equal?
  (option-map
    (binding2-list-resolve 
      (list 
        (binding2 (list number-type) boolean-type #`number->boolean #t)
        (binding2 (list string-type) boolean-type #`string->boolean #t))
      (list (syntax-with-type #`a string-type)))
    syntax-typed-datum)
  (typed `(string->boolean a) boolean-type))

(check-equal?
  (binding2-list-resolve 
    (list 
      (binding2 (list number-type) boolean-type #`number->boolean #t)
      (binding2 (list string-type) boolean-type #`string->boolean #t))
    (list (syntax-with-type #`a boolean-type)))
  #f)
