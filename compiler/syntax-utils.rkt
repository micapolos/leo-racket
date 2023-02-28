#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/sourced
  racket/syntax-srcloc)

(define (make-syntax ($srcloc : srcloc) ($datum : (Sexpof Syntax))) : Syntax
  (datum->syntax #f $datum 
    (vector
      (srcloc-source $srcloc)
      (srcloc-line $srcloc)
      (srcloc-column $srcloc)
      (srcloc-position $srcloc)
      (srcloc-span $srcloc))))

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

(define test-srcloc (srcloc "test.leo" 1 2 3 4))
(define srcloc-a (srcloc "a.leo" 1 2 3 4))
(define srcloc-b (srcloc "b.leo" 1 2 3 4))
(define srcloc-c (srcloc "c.leo" 1 2 3 4))
(define srcloc-d (srcloc "d.leo" 1 2 3 4))

(define test-syntax (make-syntax test-srcloc `test))
(define syntax-a (make-syntax srcloc-a `a))
(define syntax-b (make-syntax srcloc-b `b))
(define syntax-c (make-syntax srcloc-c `c))
(define syntax-d (make-syntax srcloc-d `d))

(define identifier-a (make-identifier srcloc-a `a))
(define identifier-b (make-identifier srcloc-b `b))
(define identifier-c (make-identifier srcloc-c `c))
(define identifier-d (make-identifier srcloc-d `d))
