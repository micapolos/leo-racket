#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/sourced
  leo/compiler/srcloc
  racket/syntax-srcloc)

(define (make-syntax ($srcloc : (Option srcloc)) ($datum : (Sexpof Syntax))) : Syntax
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

(define test-syntax (make-syntax test-srcloc `test))
(define syntax-a (make-syntax srcloc-a `a))
(define syntax-b (make-syntax srcloc-b `b))
(define syntax-c (make-syntax srcloc-c `c))
(define syntax-d (make-syntax srcloc-d `d))
