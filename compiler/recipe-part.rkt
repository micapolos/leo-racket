#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/ingredients
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/type)

(data recipe-part
  (lhs-structure : Structure)
  (rhs-structure-option : (Option Structure)))

(define null-recipe-part 
  (recipe-part null #f))
