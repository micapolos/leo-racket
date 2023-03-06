#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/expressions
  leo/compiler/expression)

(define-type Packet (U Expressions Tuple))
