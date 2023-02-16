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

(define pi-binding
  (constant-binding 
    `pi 
    number-type 
    #`pi))

(define +-binding
  (function-binding 
    `plus 
    (list number-type number-type) 
    number-type
    #`+))

(define --binding
  (function-binding 
    `minus 
    (list number-type number-type) 
    number-type
    #`-))

(define *-binding
  (function-binding 
    `times
    (list number-type number-type) 
    number-type
    #`*))

(define string-append-binding
  (function-binding 
    `plus 
    (list string-type string-type) 
    string-type
    #`string-append))

(define string-length-binding
  (function-binding 
    `length 
    (list string-type) 
    string-type
    #`string-length))

(define prelude-binding-list
  (list
    pi-binding
    +-binding
    --binding
    *-binding
    string-append-binding
    string-length-binding))

(check-equal?
  (syntax-typed-datum
    (binding-list-resolve
      prelude-binding-list
      (datum->syntax #f 
        (list 
          `plus
          (syntax-with-type #`"a" string-type)
          (syntax-with-type #`"b" string-type)))))
  (typed
    `(string-append "a" "b")
    string-type))
