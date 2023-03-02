#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/type)

(struct binding (
  (type : Type)
  (identifier : Identifier))
  #:transparent
  #:type-name Binding)
