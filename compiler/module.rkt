#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/scope
  leo/compiler/expression)

(data module (scope : Scope) (tuple : Tuple))

(define null-module (module null-scope null-tuple))
