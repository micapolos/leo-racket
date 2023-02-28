#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
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
  (param-type-stack : (Stackof Type))
  (return-type : Type)
  (identifier : Identifier)
  (function? : Boolean))

; ----------------------------------------------------------------------------

(define 
  (binding-resolve 
    ($binding : Binding) 
    ($syntax-stack : (Stackof Syntax))) 
  : (Option Syntax)
  (and
    (types-match?
      (map syntax-type $syntax-stack)
      (binding-param-type-stack $binding))
    (syntax-with-type
      (datum->syntax #f
        (if (binding-function? $binding)
          `(,(binding-identifier $binding)
            ,@(reverse (filter syntax-is-dynamic? $syntax-stack)))
          (binding-identifier $binding)))
      (binding-return-type $binding))))

(check-equal?
  (option-map
    (binding-resolve 
      (binding (stack number-type string-type) boolean-type #`foo #f)
      (stack (syntax-with-type #`a number-type) (syntax-with-type #`b string-type)))
    syntax-typed-datum)
  (typed `foo boolean-type))

(check-equal?
  (option-map
    (binding-resolve 
      (binding (stack number-type string-type) boolean-type #`foo #t)
      (stack (syntax-with-type #`a number-type) (syntax-with-type #`b string-type)))
    syntax-typed-datum)
  (typed `(foo a b) boolean-type))

(check-equal?
  (binding-resolve 
    (binding (stack number-type string-type) boolean-type #`foo #f)
    (stack (syntax-with-type #`a number-type) (syntax-with-type #`b number-type)))
  #f)

; ----------------------------------------------------------------------------

(define
  (binding-stack-resolve
    ($binding-stack : (Stackof Binding))
    ($syntax-stack : (Stackof Syntax)))
  : (Option Syntax)
  (and
    (not (null? $binding-stack))
    (or
      (binding-resolve (car $binding-stack) $syntax-stack)
      (binding-stack-resolve (cdr $binding-stack) $syntax-stack))))

(check-equal?
  (option-map
    (binding-stack-resolve 
      (stack 
        (binding (stack number-type) boolean-type #`number->boolean #t)
        (binding (stack string-type) boolean-type #`string->boolean #t))
      (stack (syntax-with-type #`a number-type)))
    syntax-typed-datum)
  (typed `(number->boolean a) boolean-type))

(check-equal?
  (option-map
    (binding-stack-resolve 
      (stack 
        (binding (stack number-type) boolean-type #`number->boolean #t)
        (binding (stack string-type) boolean-type #`string->boolean #t))
      (stack (syntax-with-type #`a string-type)))
    syntax-typed-datum)
  (typed `(string->boolean a) boolean-type))

(check-equal?
  (binding-stack-resolve 
    (stack 
      (binding (stack number-type) boolean-type #`number->boolean #t)
      (binding (stack string-type) boolean-type #`string->boolean #t))
    (stack (syntax-with-type #`a boolean-type)))
  #f)
