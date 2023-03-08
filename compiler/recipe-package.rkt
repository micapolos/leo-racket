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
  (arrow-expressions-option : (Option Expressions)))

(define null-recipe-package 
  (recipe-package null #f))

(define (recipe-package-arrow-expressions ($recipe-package : Recipe-Package)) : Expressions
  (or
    (recipe-package-arrow-expressions-option $recipe-package)
    (error "recipe must have a does body")))