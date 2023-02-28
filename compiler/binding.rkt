#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/type
  leo/typed/stack
  leo/typed/base)

(data binding
  (param-type-stack : (Stackof Type))
  (return-type : Type)
  (identifier : Identifier)
  (function? : Boolean))
