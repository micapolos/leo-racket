#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/binding)

(define-type Scope (Stackof Binding))

(define null-scope null)
