#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing)

(define-type Token (U Open Close Syntax))

(data open (symbol : Symbol))

(data close)
