#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/stack)

(define-type Body (Stackof Syntax))

(define body stack)
