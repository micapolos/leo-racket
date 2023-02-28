#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/binding)

(struct library (
  ($binding-stack : (Stackof Binding))
  ($syntax-stack : (Stackof Syntax)))
  #:transparent
  #:type-name Library)
