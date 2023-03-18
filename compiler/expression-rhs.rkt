#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/expression
  leo/compiler/type)

(define (boolean-syntax-rhs ($syntax : Syntax)) : Tuple
  (tuple
    (expression $syntax (choice! (field! `true) (field! `false)))))
