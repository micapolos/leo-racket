#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/compiler/expressions)

(define-type Package (Stackof Expressions))

(define null-package null)

(define package stack)
