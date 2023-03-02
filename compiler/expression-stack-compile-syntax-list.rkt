#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/expression)

(define (expression-stack-compile-syntax-list
  ($expression-stack : (Stackof Expression))
  ($syntax-list : (Listof Syntax)))
  : (Stackof Expression)
  (error "TODO"))
