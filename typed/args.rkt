#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/typed
  leo/typed/types
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/testing)

(struct args ((syntaxes : (Listof Syntax)) (type : Type))
  #:transparent
  #:type-name Args)

(define (args-syntax (args : Args)) : Syntax
  (let ((dynamic-syntaxes (filter syntax-is-dynamic? (args-syntaxes args))))
    (syntax-with-type
      (cond
        ((null? dynamic-syntaxes) #`())
        ((null? (cdr dynamic-syntaxes)) (car dynamic-syntaxes))
        (else (datum->syntax #f (cons `vector dynamic-syntaxes))))
      (args-type args))))

(check-equal?
  (syntax-typed-datum 
    (args-syntax 
      (args 
        (list
          (syntax-with-type #`1 number-type)) 
        (field-type `foo (struct-type-body (list number-type))))))
  (typed 
    1
    (field-type `foo (struct-type-body (list number-type)))))

(check-equal?
  (syntax-typed-datum 
    (args-syntax 
      (args 
        (list
          (syntax-with-type #`1 number-type)
          (syntax-with-type #`"foo" string-type)) 
        (field-type `foo (struct-type-body (list number-type string-type))))))
  (typed 
    `(vector 1 "foo")
    (field-type `foo (struct-type-body (list number-type string-type)))))

(check-equal?
  (syntax-typed-datum 
    (args-syntax 
      (args 
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
