#lang typed/racket/base

(provide (all-defined-out))

(require leo/compiler/binding)

(struct definition (
  (binding : Binding)
  (syntax-option : (Option Syntax)))
  #:transparent
  #:type-name Definition)
