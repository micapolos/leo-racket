#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/binding)

(data scope (binding-stack : (Stackof Binding)))
