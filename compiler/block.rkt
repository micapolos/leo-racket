#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/stack
  leo/compiler/type)

(struct block (
  ($syntax-stack : (Stackof Syntax))
  ($type-stack : (Stackof Type)))
  #:transparent
  #:type-name Block)
