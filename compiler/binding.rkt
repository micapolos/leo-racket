#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack)

(data binding
  (identifier : Identifier)
  (syntax : Syntax))

(define-type Scope (Stackof Binding))

(define scope : (-> Binding * Scope) stack)
