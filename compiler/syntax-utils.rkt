#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/sexp-utils
  leo/compiler/sourced
  leo/typed/srcloc
  leo/typed/testing)

(define (make-syntax ($datum : (Sexpof Syntax)) ($srcloc : (Option srcloc) #f)) : Syntax
  (datum->syntax #f $datum 
    (and $srcloc
      (vector
        (srcloc-source $srcloc)
        (srcloc-line $srcloc)
        (srcloc-column $srcloc)
        (srcloc-position $srcloc)
        (srcloc-span $srcloc)))))

(define (syntax-sourced ($syntax : Syntax)) : (Sourced Sexp)
  (sourced 
    (syntax->datum $syntax) 
    (syntax-srcloc $syntax)))

(define (syntax-e-with-srcloc ($syntax : Syntax))
  (with-srcloc (syntax-srcloc $syntax) (lambda () (syntax-e $syntax))))

(define null-syntax (make-syntax null-sexp))
(define static-syntax (datum->syntax #f `static))
(define dynamic-syntax (datum->syntax #f `(dynamic)))
(define test-syntax (make-syntax `test test-srcloc))

(define atomic-syntax-a (make-syntax `atomic-a srcloc-a))
(define atomic-syntax-b (make-syntax `atomic-b srcloc-b))
(define atomic-syntax-c (make-syntax `atomic-c srcloc-c))
(define atomic-syntax-d (make-syntax `atomic-d srcloc-d))

(define dynamic-syntax-a (make-syntax `(dynamic-a) srcloc-a))
(define dynamic-syntax-b (make-syntax `(dynamic-b) srcloc-a))
(define dynamic-syntax-c (make-syntax `(dynamic-c) srcloc-a))
(define dynamic-syntax-d (make-syntax `(dynamic-d) srcloc-a))

(define syntax-a dynamic-syntax-a)
(define syntax-b dynamic-syntax-b)
(define syntax-c dynamic-syntax-c)
(define syntax-d dynamic-syntax-d)

(define (syntax-static? ($syntax : Syntax)) : Boolean
  (bind $e (syntax-e $syntax)
    (or 
      (null? $e) 
      (symbol? $e) 
      (boolean? $e) 
      (number? $e) 
      (string? $e)
      (keyword? $e))))

(define (syntax-dynamic? ($syntax : Syntax)) : Boolean
  (not (syntax-static? $syntax)))

(check-not (syntax-dynamic? #`()))
(check-not (syntax-dynamic? #`foo))
(check-not (syntax-dynamic? #`#t))
(check-not (syntax-dynamic? #`123))
(check-not (syntax-dynamic? #`#:foo))
(check-not (syntax-dynamic? #`"foo"))
(check (syntax-dynamic? #`(foo)))

(define (syntax-syntax-list ($syntax : Syntax)) : (Listof Syntax)
  (define $syntax-e (syntax-e $syntax))
  (or (and (list? $syntax-e) $syntax-e) (list $syntax)))

(define (syntax-normalize ($syntax : Syntax)) : Syntax
  (bind $syntax-e (syntax-e $syntax)
    (cond
      ((list? $syntax-e) 
        (bind $normalized-e (map syntax-normalize $syntax-e)
          (if (= (length $normalized-e) 1) 
            (car $normalized-e) 
            (make-syntax `(,@$normalized-e)))))
      (else $syntax))))

(check-equal? 
  (syntax->datum (syntax-normalize #`((1) ((2)) ((3) (3)))))
  `(1 2 (3 3)))
