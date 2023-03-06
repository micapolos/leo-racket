#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/body
  leo/compiler/expression)

(data packet
  (body : Body)
  (tuple : Tuple))
