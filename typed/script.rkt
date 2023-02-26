#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing
  (for-syntax racket/base))

(data script (line-list : (Listof Line)))

(define-type Line (U Field Native))

(data field (symbol : Symbol) (script : Script))

(data native (any : Any))

(define (syntax-script ($syntax-list : (Listof Syntax))) : Script
  (script (map syntax-line $syntax-list)))

(define (syntax-line ($syntax : Syntax)) : Line
  (let (($any (syntax-e $syntax)))
    (cond
      ((and 
        (list? $any)
        (not (null? $any))
        (symbol? (syntax-e (car $any))))
        (field (syntax-e (car $any)) (syntax-script (cdr $any))))
      (else (native (syntax->datum $syntax))))))
   
(check-equal? (syntax-line #`1) (native 1))
(check-equal? (syntax-line #`"foo") (native "foo"))
(check-equal? (syntax-line #`()) (native `()))
(check-equal? (syntax-line #`(foo)) (field `foo (script null)))
(check-equal? (syntax-line #`(foo 1 "foo")) (field `foo (script (list (native 1) (native "foo")))))
