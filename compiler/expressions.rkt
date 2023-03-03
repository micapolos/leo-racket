#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/compiler/type)

(data expressions 
  ($syntax : Syntax) 
  ($type-stack : (Stackof Type)))
