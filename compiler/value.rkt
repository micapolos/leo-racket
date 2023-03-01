#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/type)

(struct value ((any : Any) (type : Type))
  #:transparent
  #:type-name Value)
