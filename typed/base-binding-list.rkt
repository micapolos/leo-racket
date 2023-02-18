#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/binding
  leo/typed/type
  leo/typed/types
  leo/typed/typed
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/testing)

(define binding:pi
  (constant-binding 
    `pi
    number-type 
    #`pi))

(define binding:+
  (function-binding 
    `plus 
    (list number-type number-type) 
    number-type
    #`+))

(define binding:-
  (function-binding 
    `minus 
    (list number-type number-type) 
    number-type
    #`-))

(define binding:*
  (function-binding 
    `times
    (list number-type number-type) 
    number-type
    #`*))

(define binding:number->string
  (function-binding 
    `string 
    (list number-type) 
    string-type
    #`number->string))

(define binding:string-append
  (function-binding 
    `plus 
    (list string-type string-type) 
    string-type
    #`string-append))

(define binding:string-length
  (function-binding 
    `length 
    (list string-type) 
    number-type
    #`string-length))

(define base-binding-list
  (list
    binding:pi
    binding:+
    binding:-
    binding:*
    binding:number->string
    binding:string-append
    binding:string-length))

(check-equal?
  (syntax-typed-datum (binding-list-apply-symbol base-binding-list `pi))
  (typed `pi number-type))

(check-equal?
  (syntax-typed-datum
    (binding-list-apply-symbol-args
      base-binding-list
      `plus
      (list
        (syntax-with-type #`a string-type)
        (syntax-with-type #`b string-type))))
  (typed `(string-append a b) string-type))