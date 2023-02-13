#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/testing
  leo/typed/type
  leo/typed/type-is-static
  leo/typed/types)

(struct (V) typed (($value : V) ($type : Type))
  #:transparent
  #:type-name Typed)

(define (syntax-typed ($syntax : (Syntaxof Any)) ($type : Type))
  (syntax-property $syntax `type $type))

(define (syntax-type ($syntax : (Syntaxof Any))) : Type
  (define $value (syntax-property $syntax `type))
  (cond
    ((equal? $value #f) (error (format "Non-typed ~s" $syntax)))
    (else (cast $value Type))))

(define (syntax-is-static? ($syntax : (Syntaxof Any))) : Boolean
  (type-is-static? (syntax-type $syntax)))

(define (syntax-is-dynamic? ($syntax : (Syntaxof Any))) : Boolean
  (not (syntax-is-static? $syntax)))

(define (syntax-typed-datum ($syntax : (Syntaxof Any)))
  (typed (syntax->datum $syntax) (syntax-type $syntax)))

(check-equal?
  (syntax->datum (syntax-typed #`1 number-type)) 
  1)

(check-equal?
  (syntax-type (syntax-typed #`1 number-type)) 
  number-type)

(define (syntax-parse ($syntax : (Syntaxof Any))) : (Option (Syntaxof Any))
  (let (($datum (syntax-e $syntax)))
    (cond
      ((boolean? $datum) 
        (syntax-typed $syntax boolean-type))
      ((number? $datum) 
        (syntax-typed $syntax number-type))
      ((string? $datum)
        (syntax-typed $syntax string-type))
      ((symbol? $datum)
        (syntax-typed #`() (field-type $datum void-type-body)))
      ((and (pair? $datum) (list? $datum))
        (let (($car (car $datum))
              ($cdr (cast (cdr $datum) (Listof (Syntaxof Any)))))
          (cond
            ((identifier? $car)
              (typed-field-syntax $car $cdr))
            (else #f))))
      (else #f))))

(check-equal?
  (syntax-typed-datum (option-ref (syntax-parse #`1))) 
  (typed 1 number-type))

(check-equal?
  (syntax-typed-datum (option-ref (syntax-parse #`"foo"))) 
  (typed "foo" string-type))

(check-equal? 
  (syntax-typed-datum (option-ref (syntax-parse #`#f)))
  (typed #f boolean-type))

(check-equal? 
  (syntax-typed-datum (option-ref (syntax-parse #`foo)))
  (typed `() (field-type `foo void-type-body)))

(define 
  (typed-field-syntax
    ($identifier : Identifier) 
    ($syntaxes : (Listof (Syntaxof Any)))) : (Syntaxof Any)
  (syntax-typed
    (let (($syntaxes (filter syntax-is-dynamic? $syntaxes)))
      (cond 
        ((null? $syntaxes) #`())
        ((null? (cdr $syntaxes)) (car $syntaxes))
        (else #`(immutable-vector #,@$syntaxes))))
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
        (syntax-typed #`1 number-type))))
  (typed 
    1 
    (field-type `x
      (struct-type-body 
        (list number-type)))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax #`tuple 
      (list 
        (syntax-typed #`1 number-type)
        (syntax-typed #`"foo" string-type))))
  (typed 
    `(immutable-vector 1 "foo") 
    (field-type `tuple 
      (struct-type-body 
        (list number-type string-type)))))

(check-equal?
  (syntax-typed-datum 
    (typed-field-syntax #`tuple 
      (list 
        (syntax-typed #`1 number-type)
        (syntax-typed #`"foo" string-type))))
  (typed 
    `(immutable-vector 1 "foo") 
    (field-type `tuple 
      (struct-type-body 
        (list number-type string-type)))))
