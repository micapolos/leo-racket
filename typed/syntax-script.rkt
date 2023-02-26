#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/racket
  leo/typed/script
  leo/typed/testing)

(define (syntax-line ($syntax : Syntax)) : Line
  (let (($any (syntax-e $syntax)))
    (cond
      ((and 
        (list? $any)
        (not (null? $any))
        (symbol? (syntax-e (car $any))))
        (field
          (syntax-e (car $any)) 
          (map syntax-line (cdr $any))))
      (else (racket (syntax->datum $syntax))))))
   
(check-equal? 
  (syntax-line #`1) 
  (racket 1))

(check-equal? 
  (syntax-line #`"foo") 
  (racket "foo"))

(check-equal? 
  (syntax-line #`()) 
  (racket `()))

(check-equal? 
  (syntax-line #`(foo)) 
  (field `foo null))

(check-equal? 
  (syntax-line #`(foo 1 "foo")) 
  (field `foo (list (racket 1) (racket "foo"))))
