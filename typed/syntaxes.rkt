#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/values
  leo/typed/testing)

(define null-syntax (datum->syntax #f null-value))

(check-equal? (syntax->datum null-syntax) null-value)
