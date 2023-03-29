#lang leo/typed

(require 
  leo/compiler/ingredients
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/expressions-sexp
  leo/compiler/expressions-utils)

(define (ingredients-sexp ($ingredients : Ingredients)) : Sexp
  `(ingredients
    ,@(reverse 
      (map expressions-sexp $ingredients))))

(check-equal?
  (ingredients-sexp
    (ingredients expressions-ab expressions-cd))
  `(ingredients
    (expressions ab (structure (a racket) (b racket)))
    (expressions cd (structure (c racket) (d racket)))))
