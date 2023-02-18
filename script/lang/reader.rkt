#lang racket/base

(require 
  syntax/strip-context
  leo/script/core/syntax)
 
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
      (require
        racket/bool
        racket/function
        leo/script/lang/base
        leo/testing
        (for-syntax racket/base))
      #,@(read-leo-stxs port src))))
