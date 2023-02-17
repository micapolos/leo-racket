#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/testing
  leo/typed/option)

(define (any-syntax (any : Any)) : Syntax
  (if (syntax? any)
    (let ((e (syntax-e any)))
      (datum->syntax #f 
        (cond
          ((symbol? e) e)
          ((boolean? e) e)
          ((number? e) e)
          ((string? e) e)
          ((list? e) (map any-syntax e))
          ((pair? e) (cons (any-syntax (car e)) (any-syntax (cdr e))))
          (else (error (format "Not a syntassx: ~v" e))))))
    (error (error (format "Not a syntaffx: ~v" any)))))

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

(define-type Thunk (Pairof Identifier (Listof Syntax)))

(define (syntax-identifier-option ($syntax : Syntax)) : (Option Identifier)
  (cond
    ((identifier? $syntax) $syntax)
    (else #f)))

(check-equal?
  (option-bind
    (syntax-identifier-option #`foo) $identifier
      (syntax->datum $identifier))
  `foo)

(check-equal?
  (option-bind
    (syntax-identifier-option #`(foo)) $identifier
      (syntax->datum $identifier))
  #f)

(define (syntax-thunk-option ($syntax : Syntax)) : (Option (Syntaxof Thunk))
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((null? $syntax-e) #f)
      ((and (list? $syntax-e) (identifier? (car $syntax-e))) $syntax)
      (else #f))))

(define (syntax-identifier-args? ($syntax : Syntax))
  (let ((e (syntax-e $syntax)))
    (and (list? e) (not (null? e)) (identifier? (car e)))))

(define (syntax-symbol-args? ($syntax : Syntax) ($symbol : Symbol))
  (let ((e (syntax-e $syntax)))
    (and 
      (list? e)
      (not (null? e))
      (identifier? (car e))
      (equal? (syntax-e (car e)) $symbol))))

(define (syntax-symbol-arg? ($syntax : Syntax) ($symbol : Symbol))
  (let ((e (syntax-e $syntax)))
    (and 
      (list? e)
      (not (null? e))
      (identifier? (car e))
      (equal? (syntax-e (car e)) $symbol)
      (not (null? (cdr e)))
      (null? (cddr e)))))

(define (syntax-symbol-arg-arg? ($syntax : Syntax) ($symbol : Symbol))
  (let ((e (syntax-e $syntax)))
    (and 
      (list? e)
      (not (null? e))
      (identifier? (car e))
      (equal? (syntax-e (car e)) $symbol)
      (not (null? (cdr e)))
      (not (null? (cdr (cdr e))))
      (null? (cdddr e)))))

(define (syntax-symbol-arg-args? ($syntax : Syntax) ($symbol : Symbol))
  (let ((e (syntax-e $syntax)))
    (and 
      (list? e)
      (not (null? e))
      (identifier? (car e))
      (equal? (syntax-e (car e)) $symbol)
      (not (null? (cdr e)))
      (not (null? (cdr (cdr e)))))))

(check-equal? 
  (option-bind
    (syntax-thunk-option #`(foo 1 2)) $thunk
      (syntax->datum $thunk))
  `(foo 1 2))

(check-equal? 
  (option-bind
    (syntax-thunk-option #`123) $thunk
      (syntax->datum $thunk))
  #f)

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
