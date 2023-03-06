#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/scope
  leo/compiler/packet
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expression-resolve)

(define (packet-expressions ($packet : Packet)) : Expressions
  (cond
    ((expressions? $packet) $packet)
    (else (tuple-expressions $packet))))

(define (packet-do ($packet : Packet) ($fn : (-> Scope Packet))) : Packet
  (define $expressions-fn (compose packet-expressions $fn))
  (cond
    ((expressions? $packet) (expressions-do $packet $expressions-fn))
    (else (tuple-do $packet $expressions-fn))))
