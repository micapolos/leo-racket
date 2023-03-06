#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/body
  leo/compiler/body-sexp
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/packet)

(define (tuple-packet ($tuple : Tuple)) : Packet
  (packet null-body $tuple))

(define (packet-values-body ($packet : Packet)) : Body
  (define $body (packet-body $packet))
  (define $tuple (packet-tuple $packet))
  (define $values-syntax (tuple-values-syntax $tuple))
  (push $body $values-syntax))

(check-equal?
  (body-sexp
    (packet-values-body
      (packet (body #`foo #`bar) (tuple expression-c expression-d))))
  `(body foo bar (values c d)))
