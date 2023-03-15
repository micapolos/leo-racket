#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/select-expression-utils
  leo/compiler/type)

(data select-ingredients
  (selection : (Option (Pairof Exact-Nonnegative-Integer Syntax)))
  (structure : Structure))

(define null-select-ingredients 
  (select-ingredients #f null-structure))

(define (select-ingredients-plus-not 
  ($ingredients : Select-Ingredients)
  ($type : Type)) : Select-Ingredients
  (select-ingredients
    (select-ingredients-selection $ingredients)
    (push
      (select-ingredients-structure $ingredients)
      $type)))

(define (select-ingredients-plus-the 
  ($ingredients : Select-Ingredients)
  ($expression : Expression)) : Select-Ingredients
  (unless (not (select-ingredients-selection $ingredients))
    (error "already selected"))
  (select-ingredients
    (pair 
      (length (select-ingredients-structure $ingredients))
      (expression-syntax $expression))
    (push
      (select-ingredients-structure $ingredients)
      (expression-type $expression))))

(define (select-ingredients-expression
  ($ingredients : Select-Ingredients)) : Expression
  (define $selection (select-ingredients-selection $ingredients))
  (unless $selection (error "not selected"))
  (index-syntax-structure-select-expression 
    (car $selection)
    (cdr $selection)
    (select-ingredients-structure $ingredients)))
