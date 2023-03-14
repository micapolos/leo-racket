#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/expression
  leo/compiler/expressions)

(define-type Expressions-Part (U Expressions Tuple))

(define null-expressions-part null-tuple)
