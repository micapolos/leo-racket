#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/compiler/type
  leo/compiler/package
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/type)

(data recipe-package
  (lhs-structure : Structure)
  (rhs-structure-option : (Option Structure)))

(define null-recipe-package 
  (recipe-package null #f))
