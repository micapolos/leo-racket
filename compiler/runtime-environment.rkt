#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/environment)

(define runtime-environment
  (environment-require
    base-environment
    `leo/lang/runtime))
