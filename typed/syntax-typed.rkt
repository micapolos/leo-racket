#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/option
  leo/typed/testing
  leo/typed/type
  leo/typed/syntax-type
  leo/typed/typed
  leo/typed/type-utils
  leo/typed/syntaxes
  leo/typed/syntax-match
  leo/typed/values
  leo/typed/types)

(define (syntax-is-static? ($syntax : Syntax)) : Boolean
  (type-is-static? (syntax-type $syntax)))

(define (syntax-is-dynamic? ($syntax : Syntax)) : Boolean
  (not (syntax-is-static? $syntax)))

(define (syntax-typed-datum ($syntax : Syntax))
  (typed (syntax->datum $syntax) (syntax-type $syntax)))

(check-equal?
  (syntax->datum (syntax-with-type #`1 number-type)) 
  1)

(check-equal?
  (syntax-type (syntax-with-type #`1 number-type)) 
  number-type)

(define 
  (typed-field-syntax
    ($symbol : Symbol) 
    ($syntaxes : (Listof Syntax))) : Syntax
  (syntax-with-type
    (syntax-list-syntax $syntaxes)
    (tuple $symbol (map syntax-type $syntaxes))))

(define (syntax-list-syntax ($syntax-list : (Listof Syntax))) : Syntax
  (let* (($dynamic-syntax-list (filter syntax-is-dynamic? $syntax-list))
         ($size (length $dynamic-syntax-list)))
    (case $size
      ((0) null-syntax)
      ((1) (car $dynamic-syntax-list))
      ((2) 
        (datum->syntax #f 
          (list #`cons (car $dynamic-syntax-list) (cadr $dynamic-syntax-list))))
      (else (datum->syntax #f (cons #`vector $dynamic-syntax-list))))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax `foo null))
  (typed null-value (tuple `foo null)))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax `x
      (list 
        (syntax-with-type #`1 number-type))))
  (typed 
    1
    (tuple `x (list number-type))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax `tuple 
      (list 
        (syntax-with-type #`a number-type)
        (syntax-with-type #`b string-type))))
  (typed 
    `(cons a b) 
    (tuple `tuple (list number-type string-type))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax `tuple 
      (list 
        (syntax-with-type #`a number-type)
        (syntax-with-type #`b string-type)
        (syntax-with-type #`c boolean-type))))
  (typed 
    `(vector a b c) 
    (tuple `tuple (list number-type string-type boolean-type))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax `tuple 
      (list 
        (syntax-with-type #`a number-type)
        (syntax-with-type #`b string-type))))
  (typed 
    `(cons a b) 
    (tuple `tuple (list number-type string-type))))
