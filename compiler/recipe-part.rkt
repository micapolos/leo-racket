#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/compiler/type
  leo/compiler/expressions-part
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/type)

(data recipe-part
  (lhs-structure : Structure)
  (rhs-structure-option : (Option Structure)))

(define null-recipe-part 
  (recipe-part null #f))