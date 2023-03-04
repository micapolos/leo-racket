#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/binding
  leo/compiler/binding-utils
  leo/compiler/package
  leo/compiler/expression
  leo/compiler/expression-resolve)

(define (binding-resolve-tuple ($binding : Binding) ($tuple : Tuple)) : (Option Package)
  (expression-resolve-tuple (binding-expression $binding) $tuple))
