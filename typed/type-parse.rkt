#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/option
  leo/typed/type
  leo/typed/types
  leo/typed/syntax-match
  leo/typed/testing)

(define (any-parse-type ($any : Any)) : Type
  (syntax-parse-type (cast-syntax (datum->syntax #f $any))))

(define (syntax-parse-type ($syntax : Syntax)) : Type
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((equal? $syntax-e `thing) (thing))
      ((equal? $syntax-e `boolean) boolean-type)
      ((equal? $syntax-e `string) string-type)
      ((equal? $syntax-e `number) number-type)
      ((equal? $syntax-e `fixnum) fixnum-type)
      ((equal? $syntax-e `flonum) flonum-type)
      ((symbol? $syntax-e) $syntax-e)
      (else
        (or
          (syntax-parse-giving $syntax)
          (syntax-parse-any $syntax)
          (cond
            ((list? $syntax-e) (map syntax-parse-type $syntax-e))
            (else $syntax-e)))))))

(define (syntax-parse-giving ($syntax : Syntax)) : (Option Type)
  (syntax-symbol-match-args-arg $syntax `giving args arg
    (giving
      (map syntax-parse-type args)
      (list (syntax-parse-type arg)))))

(define (syntax-parse-any ($syntax : Syntax)) : (Option Type)
  (syntax-symbol-match-arg $syntax `any arg
    (any (syntax-parse-type arg))))

(check-equal? (syntax-parse-type #`boolean) boolean-type)
(check-equal? (syntax-parse-type #`number) number-type)
(check-equal? (syntax-parse-type #`fixnum) fixnum-type)
(check-equal? (syntax-parse-type #`flonum) flonum-type)
(check-equal? (syntax-parse-type #`string) string-type)
(check-equal? (syntax-parse-type #`foo) `foo)

(check-equal? 
  (syntax-parse-type #`(foo boolean number string)) 
  `(foo ,boolean-type ,number-type ,string-type))

(check-equal?
  (syntax-parse-type #`(id (first number) (second string)))
  `(id
    (first ,number-type)
    (second ,string-type)))

(check-equal?
  (syntax-parse-type #`(giving number string boolean))
  (giving (list number-type string-type) (list boolean-type)))

(check-equal?
  (any-parse-type `(giving number string boolean))
  (giving (list number-type string-type) (list boolean-type)))

(check-equal?
  (any-parse-type `(any number))
  (any number-type))
