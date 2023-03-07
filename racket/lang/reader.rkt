#lang racket/base

(require 
  syntax/strip-context
  racket/port
  racket/function
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
    #`(module leo racket/base
      (require racket/unsafe/ops)
      #,(leo-compile-any-list
        (port->list 
          (curry read-syntax src) 
          port)))))
