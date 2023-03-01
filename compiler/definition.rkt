#lang typed/racket/base

(provide (all-defined-out))

(require leo/compiler/expression)

(struct definition (
  (expression : Expression)
  (syntax-option : (Option Syntax)))
  #:transparent
  #:type-name Definition)
