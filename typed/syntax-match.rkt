#lang typed/racket

(provide (all-defined-out))

(require 
  leo/testing
  leo/typed/option)

(define (cast-syntax-any ($any : Any)) : (Syntaxof Any)
  (if (syntax? $any) $any (error (format "not a syntax ~v" $any))))

(define (cast-syntax ($syntax : (Syntaxof Any))) : Syntax
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((symbol? $syntax-e) (datum->syntax $syntax $syntax-e))
      ((number? $syntax-e) (datum->syntax $syntax $syntax-e))
      ((string? $syntax-e) (datum->syntax $syntax $syntax-e))
      ((list? $syntax-e)
        (datum->syntax $syntax 
          (map cast-syntax (map cast-syntax-any $syntax-e))))
      (else (error (format "not a syntax ~v" $syntax))))))

(check-equal? (syntax->datum (cast-syntax #`foo)) `foo)
(check-equal? (syntax->datum (cast-syntax #`1)) 1)
(check-equal? (syntax->datum (cast-syntax #`"foo")) "foo")
(check-equal? (syntax->datum (cast-syntax #`(point 1 2))) `(point 1 2))

(define (cast-syntaxes ($list : (Listof Any))) : (Listof Syntax)
  (map cast-syntax (map cast-syntax-any $list)))

(define 
  #:forall (T)
  (syntax-match-list 
    ($syntax : Syntax) 
    ($fn : (-> (Listof Syntax) (Option T)))) : (Option T)
  (let (($syntax-e (syntax-e $syntax)))
    (if (list? $syntax-e) ($fn $syntax-e) #f)))

(check-equal?
  (syntax-match-list 
    #`(point 1 "foo")
    (lambda (($syntaxes : (Listof Syntax)))
      (map syntax->datum $syntaxes)))
  `(point 1 "foo"))

(check-equal?
  (syntax-match-list 
    #`123
    (lambda (($syntaxes : (Listof Syntax)))
      (map syntax->datum $syntaxes)))
  #f)

(define 
  #:forall (T)
  (syntax-match-symbol-rhs
    ($syntax : Syntax)
    ($symbol : Symbol)
    ($fn : (-> (Listof Syntax) (Option T)))) : (Option T)
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((null? $syntax-e) #f)
      ((list? $syntax-e)
        (let* (($syntaxes (cast-syntaxes $syntax-e))
               ($car (car $syntaxes))
               ($cdr (cdr $syntaxes)))
          (and 
            (equal? (syntax-e $car) $symbol)
            ($fn $cdr))))
      (else #f))))

(check-equal?
  (syntax-match-symbol-rhs #`(point 1 2) `point
    (lambda ((rhs : (Listof Syntax))) 
      (map syntax->datum rhs)))
  `(1 2))
