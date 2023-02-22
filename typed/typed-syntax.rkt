#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/typed/syntaxes
  leo/typed/type
  leo/typed/types
  leo/typed/typed
  leo/typed/testing)

(define (boolean-typed-syntax ($boolean : Boolean))
  (syntax-with-type 
    (datum->syntax #f (if $boolean `true `false)) 
    boolean-type))

(define (string-typed-syntax ($string : String))
  (syntax-with-type 
    (datum->syntax #f $string) 
    string-type))

(define (number-typed-syntax ($number : Number))
  (syntax-with-type 
    (datum->syntax #f $number) 
    number-type))

(define (fixnum-typed-syntax ($fixnum : Fixnum))
  (syntax-with-type 
    (datum->syntax #f $fixnum) 
    fixnum-type))

(define (flonum-typed-syntax ($flonum : Flonum))
  (syntax-with-type 
    (datum->syntax #f $flonum) 
    flonum-type))

(define (symbol-typed-syntax ($symbol : Symbol))
  (syntax-with-type 
    void-syntax
    (symbol-type $symbol)))

(define (field-typed-syntax ($symbol : Symbol) ($typed-syntaxes : (Listof Syntax)))
  (syntax-with-type
    (datum->syntax #f (filter syntax-is-dynamic? $typed-syntaxes))
    (field-type $symbol (struct-type-body (map syntax-type $typed-syntaxes)))))

(check-equal? 
  (syntax-typed-datum (boolean-typed-syntax #f))
  (typed `false boolean-type))

(check-equal? 
  (syntax-typed-datum (boolean-typed-syntax #t))
  (typed `true boolean-type))

(check-equal? 
  (syntax-typed-datum (string-typed-syntax "foo"))
  (typed "foo" string-type))

(check-equal? 
  (syntax-typed-datum (number-typed-syntax 3.14))
  (typed 3.14 number-type))

(check-equal? 
  (syntax-typed-datum (fixnum-typed-syntax 3))
  (typed 3 fixnum-type))

(check-equal? 
  (syntax-typed-datum (flonum-typed-syntax 3.14))
  (typed 3.14 flonum-type))

(check-equal? 
  (syntax-typed-datum (symbol-typed-syntax `foo))
  (typed `(void) (symbol-type `foo)))

(check-equal? 
  (syntax-typed-datum 
    (field-typed-syntax `foo 
      (list 
        (number-typed-syntax 1) 
        (symbol-typed-syntax `bar)
        (string-typed-syntax "foo"))))
  (typed 
    `(1 "foo") 
    (field-type `foo 
      (struct-type-body 
        (list number-type (symbol-type `bar) string-type)))))
