#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/compiler/type)

(data evaluator
  (input-value-stack : (Stackof Value))
  (output-value-stack : (Stackof Value)))
