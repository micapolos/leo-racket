#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/sourced
  leo/compiler/srcloc
  racket/syntax-srcloc)

(define (make-syntax ($datum : (Sexpof Syntax)) ($srcloc : (Option srcloc) #f)) : Syntax
  (datum->syntax #f $datum 
    (and $srcloc
      (vector
        (srcloc-source $srcloc)
        (srcloc-line $srcloc)
        (srcloc-column $srcloc)
        (srcloc-position $srcloc)
        (srcloc-span $srcloc)))))

(define (make-identifier ($srcloc : srcloc) ($symbol : Symbol)) : Identifier
  (datum->syntax #f $symbol
    (vector
      (srcloc-source $srcloc)
      (srcloc-line $srcloc)
      (srcloc-column $srcloc)
      (srcloc-position $srcloc)
      (srcloc-span $srcloc))))

(define (syntax-sourced ($syntax : Syntax)) : (Sourced Sexp)
  (sourced 
    (syntax->datum $syntax) 
    (syntax-srcloc $syntax)))

(define (syntax-srcloc ($syntax : Syntax)) : srcloc
  (srcloc
    (syntax-source $syntax)
    (syntax-line $syntax)
    (syntax-column $syntax)
    (syntax-position $syntax)
    (syntax-span $syntax)))

(define (syntax-e-with-srcloc ($syntax : Syntax))
  (with-srcloc (syntax-srcloc $syntax) (lambda () (syntax-e $syntax))))

(define test-syntax (make-syntax `test test-srcloc))
(define syntax-a (make-syntax `a srcloc-a))
(define syntax-b (make-syntax `b srcloc-b))
(define syntax-c (make-syntax `c srcloc-c))
(define syntax-d (make-syntax `d srcloc-d))
