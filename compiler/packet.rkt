#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/package
  leo/compiler/expression)

(define-type Packet (U Package Tuple))
