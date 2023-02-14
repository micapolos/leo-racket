#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/types
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/testing)

(define-type PartialBody (Option (Listof Syntax)))

(define-type PartialRhs (U Partial PartialBody))

(struct partial ((identifier : Identifier) (rhs : PartialRhs))
  #:transparent
  #:type-name Partial)

; ----------------------------------------------------------

(define (partial-type ($partial : Partial)) : Type
  (let (($rhs (partial-rhs $partial)))
    (field-type 
      (syntax-e (partial-identifier $partial))
      (cond
        ((partial? $rhs) 
          (struct-type-body (list (partial-type $rhs))))
        ((list? $rhs) 
          (struct-type-body (map syntax-type $rhs)))
        (else 
          void-type-body)))))

(check-equal?
  (partial-type (partial #`vec #f))
  (field-type `vec void-type-body))

(check-equal?
  (partial-type 
    (partial 
      #`vec 
      (list 
        (syntax-with-type #`1 number-type)
        (syntax-with-type #`"foo" string-type))))
  (field-type `vec 
    (struct-type-body 
      (list
        number-type
        string-type))))

(check-equal?
  (partial-type 
    (partial #`center
      (partial #`point
        (list 
          (syntax-with-type #`1 number-type)
          (syntax-with-type #`"foo" string-type)))))
  (field-type `center
    (struct-type-body 
      (list
        (field-type `point
          (struct-type-body
            (list
              number-type
              string-type)))))))

; ----------------------------------------------------------

(define (partial-untyped-syntax ($partial : Partial)) : Syntax
  (let (($rhs (partial-rhs $partial)))
    (cond
      ((partial? $rhs) 
        (partial-untyped-syntax $rhs))
      ((list? $rhs)
        (let (($syntaxes (filter syntax-is-dynamic? $rhs)))
          (datum->syntax #f 
            (cond
              ((null? $syntaxes) #`())
              ((equal? (length $syntaxes) 1) (car $syntaxes))
              (else (cons `immutable-vector $syntaxes))))))
      (else 
        #`()))))

(check-equal? 
  (syntax->datum (partial-untyped-syntax (partial #`vec #f)))
  `())

(check-equal?
  (syntax->datum
    (partial-untyped-syntax 
      (partial
        #`vec
        (list 
          (syntax-with-type #`1 number-type)
          (syntax-with-type #`"foo" string-type)))))
  `(immutable-vector 1 "foo"))

(check-equal?
  (syntax->datum
    (partial-untyped-syntax 
      (partial
        #`vec
        (list 
          (syntax-with-type #`() (field-type `bar void-type-body))
          (syntax-with-type #`"foo" string-type)))))
  "foo")

(check-equal?
  (syntax->datum
    (partial-untyped-syntax 
      (partial #`center
        (partial #`point
          (list 
            (syntax-with-type #`1 number-type)
            (syntax-with-type #`"foo" string-type))))))
  `(immutable-vector 1 "foo"))

; ----------------------------------------------------------

(define (partial-syntax ($partial : Partial)) : Syntax
  (syntax-with-type
    (partial-untyped-syntax $partial)
    (partial-type $partial)))

