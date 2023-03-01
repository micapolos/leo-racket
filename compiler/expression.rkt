#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/type)

(struct expression ((syntax : Syntax) (type : Type))
  #:transparent
  #:type-name Expression)
