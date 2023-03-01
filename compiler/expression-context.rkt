#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/expression
  leo/typed/stack)

(struct expression-context (
  ($private-expression-stack : (Stackof Expression))
  ($public-expression-stack : (Stackof Expression)))
  #:transparent
  #:type-name Expression-Context)
