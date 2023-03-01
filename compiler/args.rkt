#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/sourced
  leo/typed/stack
  leo/compiler/typed-syntax)

(define-type Args (Sourced (Stackof Typed-Syntax)))

(define args sourced)

(define (args-srcloc ($args : Args)) : (Option srcloc)
  (sourced-srcloc $args))

(define (args-typed-syntax-stack ($args : Args)) : (Stackof Typed-Syntax)
  (sourced-value $args))
