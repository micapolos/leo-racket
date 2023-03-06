#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/packet
  leo/compiler/package
  leo/compiler/package-sexp
  leo/compiler/packet
  leo/compiler/expression-utils)

(define (packet-sexp ($packet : Packet)) : Sexp
  (cond
    ((package? $packet) (package-sexp $packet))
    (else (tuple-sexp $packet))))
