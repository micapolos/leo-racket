#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/type
  leo/compiler/expressions)

(data match-expressions-part
  (case-stack : (Stackof Case))
  (type-list : (Listof Type)))

(data case
  (lhs-type : Type)
  (rhs-expressions : Expressions))
