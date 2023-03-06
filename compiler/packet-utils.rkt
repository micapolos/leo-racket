#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/body
  leo/compiler/expression
  leo/compiler/packet)

(define (tuple-packet ($tuple : Tuple)) : Packet
  (packet null-body $tuple))
