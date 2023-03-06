#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/packet
  leo/compiler/body
  leo/compiler/typed
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/packet)

(define (packet-sexp ($packet : Packet)) : Sexp
  `(packet
    ,(body-sexp (packet-body $packet))
    ,(tuple-sexp (packet-tuple $packet))))

(check-equal?
  (packet-sexp
    (packet 
      (body syntax-a syntax-b)
      (tuple expression-c expression-d)))
  `(packet 
    (body a b) 
    (tuple 
      (expression c (racket c)) 
      (expression d (racket d)))))
