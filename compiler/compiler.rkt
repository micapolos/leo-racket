#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/scope
  leo/compiler/expressions-part)

(data compiler 
  (scope : Scope) 
  (expressions-part : Expressions-Part))

(define null-compiler (compiler null-scope null-expressions-part))
