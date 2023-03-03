#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/base
  leo/compiler/binding
  leo/compiler/body)

(data compiler
  (binding-stack : (Stackof Binding))
  (body : Body))
