#lang leo/typed

(require
  leo/compiler/compiler
  leo/compiler/action
  leo/compiler/ingredients
  leo/compiler/ingredients-action
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/compile-recursively
  leo/compiler/syntax-type)

(define
  (compile-ingredients-action-a
    ($compiler : Compiler)
    ($syntax-list : (Listof Syntax)))
  : Ingredients-Action
  (append-action
    (ingredients
      (expression-expressions
        (reified-expression
          (syntax-list-structure $syntax-list))))))
