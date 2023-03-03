#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/type)

(data package 
  ($syntax : Syntax)
  ($structure : Structure))
