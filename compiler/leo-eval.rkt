#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/unsafe/ops
  leo/compiler/syntax-utils)

(define-namespace-anchor leo-namespace-anchor)

(define (leo-eval-syntax ($syntax : Syntax)) : AnyValues
  (eval-syntax 
    $syntax
    (namespace-anchor->namespace leo-namespace-anchor)))

(define (leo-eval ($datum : Datum)) : AnyValues
  (leo-eval-syntax (make-syntax $datum)))
