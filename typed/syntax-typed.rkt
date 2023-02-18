#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/option
  leo/testing
  leo/typed/type
  leo/typed/syntax-type
  leo/typed/typed
  leo/typed/type-utils
  leo/typed/syntax-match
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

(define (syntax->typed ($syntax : Syntax)) : Syntax
  (let (($datum (syntax-e $syntax)))
    (cond
      ((boolean? $datum) 
        (syntax-with-type $syntax boolean-type))
      ((number? $datum) 
        (syntax-with-type $syntax number-type))
      ((string? $datum)
        (syntax-with-type $syntax string-type))
      ((symbol? $datum)
        (syntax-with-type #`() (field-type $datum void-type-body)))
      ((null? $datum) (error "dupa"))
      ((list? $datum)
        (let (($car (car $datum))
              ($cdr (map syntax->typed (cdr $datum))))
          (cond
            ((identifier? $car)
              (typed-field-syntax $car $cdr))
            (else (error "jajko")))))
      (else (error "dupa")))))

(check-equal?
  (syntax-typed-datum (syntax->typed #`1))
  (typed 1 number-type))

(check-equal?
  (syntax-typed-datum (syntax->typed #`"foo"))
  (typed "foo" string-type))

(check-equal? 
  (syntax-typed-datum (syntax->typed #`#f))
  (typed #f boolean-type))

(check-equal? 
  (syntax-typed-datum (syntax->typed #`foo))
  (typed `() (field-type `foo void-type-body)))

(define 
  (typed-field-syntax
    ($identifier : Identifier) 
    ($syntaxes : (Listof Syntax))) : Syntax
  (syntax-with-type
    (let (($syntaxes (filter syntax-is-dynamic? $syntaxes))
          ($size (length $syntaxes)))
      (case $size
        ((0) #`())
        ((1) (car $syntaxes))
        ((2) (datum->syntax #f (list `cons (car $syntaxes) (cadr $syntaxes))))
        (else (datum->syntax #f (cons `vector $syntaxes)))))
    (field-type 
      (syntax-e $identifier) 
      (struct-type-body (map syntax-type $syntaxes)))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax #`foo null))
  (typed 
    `()
    (field-type `foo (struct-type-body null))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax #`x 
      (list 
        (syntax-with-type #`1 number-type))))
  (typed 
    1 
    (field-type `x
      (struct-type-body 
        (list number-type)))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax #`tuple 
      (list 
        (syntax-with-type #`a number-type)
        (syntax-with-type #`b string-type))))
  (typed 
    `(cons a b) 
    (field-type `tuple 
      (struct-type-body 
        (list number-type string-type)))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax #`tuple 
      (list 
        (syntax-with-type #`a number-type)
        (syntax-with-type #`b string-type)
        (syntax-with-type #`c boolean-type))))
  (typed 
    `(vector a b c) 
    (field-type `tuple 
      (struct-type-body 
        (list number-type string-type boolean-type)))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax #`tuple 
      (list 
        (syntax-with-type #`a number-type)
        (syntax-with-type #`b string-type))))
  (typed 
    `(cons a b) 
    (field-type `tuple 
      (struct-type-body 
        (list number-type string-type)))))
