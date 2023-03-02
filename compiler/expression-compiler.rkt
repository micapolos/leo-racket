#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/expression)

(struct expression-compiler (
  (bound-expression-stack : (Stackof Expression))
  (compiled-expression-stack : (Stackof Expression)))
  #:transparent
  #:type-name Expression-Compiler)
