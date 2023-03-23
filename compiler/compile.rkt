#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/compiler
  leo/compiler/compiler-plus-syntax
  leo/compiler/compile-recursively)

(define (compile-ingredients
  ($tuple : Tuple)
  ($syntax-list : (Listof Syntax)))
  : Ingredients
  (parameterize ((compile-ingredients-parameter compile-ingredients))
    (compiler-ingredients
      (fold
        (compiler $tuple null-tuple)
        $syntax-list
        compiler-plus-syntax))))

(define (compile-expressions
  ($tuple : Tuple)
  ($syntax-list : (Listof Syntax)))
  : Expressions
  (ingredients-expressions
    (compile-ingredients
      $tuple $syntax-list)))

