#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/typed
  leo/typed/types
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/testing)

(struct values ((syntaxes : (Listof Syntax)) (type : Type))
  #:transparent
  #:type-name Values)

(define (values-syntax (values : Values)) : Syntax
  (let ((dynamic-syntaxes (filter syntax-is-dynamic? (values-syntaxes values))))
    (syntax-with-type
      (cond
        ((null? dynamic-syntaxes) #`())
        ((null? (cdr dynamic-syntaxes)) (car dynamic-syntaxes))
        (else (datum->syntax #f (cons `vector-immutable dynamic-syntaxes))))
      (values-type values))))

(check-equal?
  (syntax-typed-datum 
    (values-syntax 
      (values 
        (list
          (syntax-with-type #`1 number-type)) 
        (field-type `foo (struct-type-body (list number-type))))))
  (typed 
    1
    (field-type `foo (struct-type-body (list number-type)))))

(check-equal?
  (syntax-typed-datum 
    (values-syntax 
      (values 
        (list
          (syntax-with-type #`1 number-type)
          (syntax-with-type #`"foo" string-type)) 
        (field-type `foo (struct-type-body (list number-type string-type))))))
  (typed 
    `(vector-immutable 1 "foo")
    (field-type `foo (struct-type-body (list number-type string-type)))))

(check-equal?
  (syntax-typed-datum 
    (values-syntax 
      (values 
        (list
          (syntax-with-type #`() (void-field-type `empty))
          (syntax-with-type #`"foo" string-type)) 
        (field-type `foo 
          (struct-type-body 
            (list 
              (field-type `empty void-type-body) 
              string-type))))))
  (typed 
    "foo"
    (field-type `foo 
      (struct-type-body 
        (list 
          (void-field-type `empty) 
          string-type)))))