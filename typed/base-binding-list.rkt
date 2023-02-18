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
    flonum-type 
    #`pi))

(define binding:fx+
  (function-binding 
    `plus 
    (list fixnum-type fixnum-type) 
    number-type
    #`unsafe-fx+))

(define binding:fl+
  (function-binding 
    `plus 
    (list flonum-type flonum-type) 
    number-type
    #`unsafe-fl+))

(define binding:fx-
  (function-binding 
    `minus 
    (list fixnum-type fixnum-type) 
    number-type
    #`unsafe-fx-))

(define binding:fl-
  (function-binding 
    `minus 
    (list flonum-type flonum-type) 
    number-type
    #`unsafe-fl-))

(define binding:fx*
  (function-binding 
    `times
    (list fixnum-type fixnum-type) 
    number-type
    #`unsafe-fx*))

(define binding:fl*
  (function-binding 
    `times
    (list flonum-type flonum-type) 
    number-type
    #`unsafe-fl*))

(define binding:fx->string
  (function-binding 
    `string 
    (list fixnum-type) 
    string-type
    #`number->string))

(define binding:fl->string
  (function-binding 
    `string 
    (list flonum-type) 
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
    fixnum-type
    #`string-length))

(define base-binding-list
  (list
    binding:pi
    binding:fx+
    binding:fl+
    binding:fx-
    binding:fl-
    binding:fx*
    binding:fl*
    binding:fx->string
    binding:fl->string
    binding:string-append
    binding:string-length))

(check-equal?
  (syntax-typed-datum (binding-list-apply-symbol base-binding-list `pi))
  (typed `pi flonum-type))

(check-equal?
  (syntax-typed-datum
    (binding-list-apply-symbol-args
      base-binding-list
      `plus
      (list
        (syntax-with-type #`a string-type)
        (syntax-with-type #`b string-type))))
  (typed `(string-append a b) string-type))
