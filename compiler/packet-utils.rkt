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

; ------------------------------------------------------------

(define (packet-values-body ($packet : Packet)) : Body
  (define $body (packet-body $packet))
  (define $tuple (packet-tuple $packet))
  (define $values-syntax (tuple-values-syntax $tuple))
  (push $body $values-syntax))

(check-equal?
  (body-sexp
    (packet-values-body
      (packet 
        (body syntax-a syntax-b) 
        (tuple expression-c expression-d))))
  `(body a b (values c d)))

; ------------------------------------------------------------

(define (packet-syntax ($packet : Packet)) : Syntax
  (define $body (packet-body $packet))
  (define $tuple (packet-tuple $packet))
  (cond
    ((null? $body)
      (tuple-values-syntax $tuple))
    (else
      (make-syntax
        `(let () ,@(reverse (packet-values-body $packet)))))))

(check-equal?
  (syntax->datum
    (packet-syntax
      (packet
        null-body
        (tuple expression-c expression-d))))
  `(values c d))

(check-equal?
  (syntax->datum
    (packet-syntax
      (packet
        (body syntax-a syntax-b) 
        (tuple expression-c expression-d))))
  `(let () a b (values c d)))
