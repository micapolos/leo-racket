#lang leo/typed

(require 
  racket/syntax-srcloc)

(define (make-identifier ($srcloc : srcloc) ($symbol : Symbol)) : Identifier
  (datum->syntax #f $symbol
    (vector
      (srcloc-source $srcloc)
      (srcloc-line $srcloc)
      (srcloc-column $srcloc)
      (srcloc-position $srcloc)
      (srcloc-span $srcloc))))

(define identifier-a (make-identifier srcloc-a `a))
(define identifier-b (make-identifier srcloc-b `b))
(define identifier-c (make-identifier srcloc-c `c))
(define identifier-d (make-identifier srcloc-d `d))
