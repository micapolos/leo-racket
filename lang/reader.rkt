#lang racket/base

(require 
  syntax/strip-context
  racket/port
  racket/function
  leo/script/core/syntax
  leo/compiler/ingredients-top-level
  leo/compiler/leo-compile)

(provide 
  (rename-out 
    (leo-read read)
    (leo-read-syntax read-syntax)))
 
(define (leo-read in)
  (syntax->datum
    (leo-read-syntax #f in)))
 
(define (leo-read-syntax src port)
  (strip-context
    #`(module leo leo/lang/runtime
      #,@(parameterize ((top-level-string? #t)
                       (read-decimal-as-inexact #f))
        (leo-compile-any-list
          (parameterize ((read-leo-compiler? #t))
            (read-leo-stxs port src)))))))
