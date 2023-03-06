#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/packet
  leo/compiler/expressions
  leo/compiler/expressions-sexp
  leo/compiler/packet
  leo/compiler/expression-utils)

(define (packet-sexp ($packet : Packet)) : Sexp
  (cond
    ((expressions? $packet) (expressions-sexp $packet))
    (else (tuple-sexp $packet))))
