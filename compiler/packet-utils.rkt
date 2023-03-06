#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/scope
  leo/compiler/packet
  leo/compiler/package
  leo/compiler/package-utils
  leo/compiler/expression-resolve)

(define (packet-package ($packet : Packet)) : Package
  (cond
    ((package? $packet) $packet)
    (else (tuple-package $packet))))

(define (packet-do ($packet : Packet) ($fn : (-> Scope Packet))) : Packet
  (define $package-fn (compose packet-package $fn))
  (cond
    ((package? $packet) (package-do $packet $package-fn))
    (else (tuple-do $packet $package-fn))))
