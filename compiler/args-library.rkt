#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/args
  leo/compiler/binding
  leo/compiler/library
  leo/compiler/typed-syntax
  leo/compiler/generate-temporary)

(define (args-library
  ($args : Args))
  : Library
  (error "TODO"))
