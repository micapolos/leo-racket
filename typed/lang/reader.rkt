#lang racket/base

(require 
  syntax/strip-context
  leo/core/syntax
  leo/typed/parse)
 
(provide 
  (rename-out 
    (leo-read read)
    (leo-read-syntax read-syntax)))
 
(define (leo-read in)
  (syntax->datum
    (leo-read-syntax #f in)))
 
(define (leo-read-syntax src port)
  (strip-context
    #`(module leo racket/base
      (provide (all-defined-out))
      (require rackunit (for-syntax racket/base))
      #,@(parse (read-leo-stxs port src)))))
