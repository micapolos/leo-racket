#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/type)

(struct binding (
  (type : Type)
  (identifier-option : (Option Identifier)))
  #:transparent
  #:type-name Binding)

(define (empty-binding ($type : Type)) : Binding
  (binding $type #f))
