#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/type
  leo/typed/stack
  leo/typed/base)

(data binding
  (type : Type)
  (syntax : Syntax))
