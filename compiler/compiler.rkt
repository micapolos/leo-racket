#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/scope
  leo/compiler/expression)

(data compiler 
  (scope : Scope) 
  (tuple : Tuple))

(define null-compiler (compiler null-scope null-tuple))
