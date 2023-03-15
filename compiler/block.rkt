#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/type)

(data block 
  (syntax : Syntax)
  (arrow : Arrow))
