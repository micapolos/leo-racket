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
    #`(module leo racket/base
      (provide (all-defined-out))
      (require 
        racket/unsafe/ops 
        leo/testing
        leo/typed/any-leo-string
        leo/typed/decompiler
        leo/typed/type-parse)
      #,@(anys-compile (read-leo-stxs port src)))))
