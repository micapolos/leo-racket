#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/compiler
  leo/compiler/program
  leo/compiler/program-compiler
  leo/compiler/compiler-plus-syntax
  leo/compiler/compile-recursively)

(define (compile-ingredients
  ($tuple : Tuple)
  ($syntax-list : (Listof Syntax)))
  : Ingredients
  (parameterize ((compile-ingredients-parameter compile-ingredients))
    (program-compiler-ingredients
      (fold
        (program-compiler $tuple null-program)
        $syntax-list
        program-compiler-plus-syntax))))

(define (compile-expressions
  ($tuple : Tuple)
  ($syntax-list : (Listof Syntax)))
  : Expressions
  (ingredients-expressions
    (compile-ingredients
      $tuple $syntax-list)))

