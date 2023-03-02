#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/binding
  leo/typed/base
  leo/typed/stack)

(data definition
  (syntax-option : (Option Syntax))
  (binding-stack : (Stackof Binding)))
