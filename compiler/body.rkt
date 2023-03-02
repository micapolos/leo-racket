#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/compiler/expression)

(struct body (
  ($define-syntax-stack : (Stackof Syntax))
  ($expression-stack : (Stackof Expression)))
  #:transparent
  #:type-name Body)
