#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/type)

(struct value ((type : Type) (any : Any))
  #:transparent
  #:type-name Value)
