#lang racket/base

(require 
  syntax/strip-context
  leo/script/core/syntax
  leo/typed/syntax-match
  leo/typed/compile)
 
(provide 
  (rename-out 
    (leo-read read)
    (leo-read-syntax read-syntax)))
 
(define (leo-read in)
  (syntax->datum
    (leo-read-syntax #f in)))
 
(define (leo-read-syntax src port)
  (strip-context
    #`(module leo racket
      (provide (all-defined-out))
      #,@(anys-compile (read-leo-stxs port src)))))
