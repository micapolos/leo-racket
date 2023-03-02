#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/compiler/expression)

(struct values (
  ($expression-stack : (Stackof Expression)))
  #:transparent
  #:type-name Values)
