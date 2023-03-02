#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/compiler/block
  leo/compiler/values)

(struct body (
  ($block : Block)
  ($values : Values))
  #:transparent
  #:type-name Body)
