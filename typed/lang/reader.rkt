#lang racket/base

(provide
  (rename-out
    (lang-read read)
    (lang-read-syntax read-syntax)))

(require
  syntax/strip-context
  racket/port
  racket/function)

(define (lang-read in)
  (syntax->datum
    (lang-read-syntax #f in)))

(define (lang-read-syntax src port)
  (strip-context
    #`(module lang typed/racket/base
      (provide (all-defined-out))
      (require
        racket/function
        racket/list
        racket/string
        leo/typed/base
        leo/typed/environment
        leo/typed/dictionary
        leo/typed/failure
        leo/typed/maybe
        leo/typed/result
        leo/typed/option
        leo/typed/parser
        leo/typed/position
        leo/typed/positioned
        leo/typed/srcloc
        leo/typed/stack
        leo/typed/testing
        leo/typed/syntax-match
        (for-syntax racket/base))
      #,@(port->list (curry read-syntax src) port))))
