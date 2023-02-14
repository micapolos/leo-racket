#lang typed/racket/base

(provide (all-defined-out))

(define (typed-transform-any (stx : Any)) : Any
  (if (syntax? stx) (typed-transform stx) (error "dupa")))

(define (typed-transform (stx : (Syntaxof Any))) stx)
