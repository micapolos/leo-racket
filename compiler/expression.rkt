#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/type)

(data expression (syntax : Syntax) (type : Type))

(define-type Tuple (Stackof Expression))

(define tuple stack)
