#lang racket/base

(require 
  syntax/strip-context
  leo/core/syntax
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
      (require racket/math leo/testing (for-syntax racket/base))
      #,@(map
        (lambda (d) (datum->syntax #f d))
        (sexps-compile 
          (map
            syntax->datum 
            (read-leo-stxs port src)))))))
