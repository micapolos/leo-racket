#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/type)

(data binding
  (type : Type)
  (identifier-option : (Option Identifier)))

(define (empty-binding ($type : Type)) : Binding
  (binding $type #f))
