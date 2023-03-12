#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/sexp-utils
  leo/compiler/sourced
  leo/typed/srcloc)

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
(define test-syntax (make-syntax `test test-srcloc))
(define syntax-a (make-syntax `a srcloc-a))
(define syntax-b (make-syntax `b srcloc-b))
(define syntax-c (make-syntax `c srcloc-c))
(define syntax-d (make-syntax `d srcloc-d))

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
