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
  leo/compiler/compile-ingredients)

(define (tuple-syntax-list-ingredients
  ($tuple : Tuple)
  ($syntax-list : (Listof Syntax)))
  : Ingredients
  (parameterize ((compile-ingredients-parameter tuple-syntax-list-ingredients))
    (compiler-ingredients
      (fold
        (compiler $tuple null-tuple)
        $syntax-list
        compiler-plus-syntax))))

(define (tuple-syntax-list-expressions
  ($tuple : Tuple)
  ($syntax-list : (Listof Syntax)))
  : Expressions
  (ingredients-expressions
    (tuple-syntax-list-ingredients
      $tuple $syntax-list)))

