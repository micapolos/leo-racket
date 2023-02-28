#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/sourced
  leo/typed/stack
  leo/compiler/typed-syntax)

(define-type Args (Sourced (Stackof Typed-Syntax)))
