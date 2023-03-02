#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/binding
  leo/typed/stack)

(struct definition (
  (syntax-option : (Option Syntax))
  (binding-stack : (Stackof Binding)))
  #:transparent
  #:type-name Definition)
