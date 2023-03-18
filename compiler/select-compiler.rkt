#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/option
  leo/typed/syntax-match
  leo/compiler/scope
  leo/compiler/syntax-type
  leo/compiler/type
  leo/compiler/compile-ingredients
  leo/compiler/expression-utils
  leo/compiler/expressions-utils
  leo/compiler/ingredients-utils
  leo/compiler/select-ingredients)

(data select-compiler
  (scope : Scope)
  (ingredients : Select-Ingredients))

(define (compile-select-ingredients
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Select-Ingredients
  (select-compiler-ingredients
    (fold
      (select-compiler $scope null-select-ingredients)
      $syntax-list
      select-compiler-plus-syntax)))

(define (select-compiler-plus-syntax
  ($compiler : Select-Compiler)
  ($syntax : Syntax))
  : Select-Compiler
  (define $scope (select-compiler-scope $compiler))
  (define $ingredients (select-compiler-ingredients $compiler))
  (or
    (syntax-match-symbol-args $syntax $symbol $syntax-list
      (cond
        ((equal? $symbol `not)
          (select-compiler
            $scope
            (select-ingredients-plus-not 
              $ingredients
              (option-or 
                (single (syntax-list-structure $syntax-list))
                (error "not must have single type")))))
        ((equal? $symbol `the)
          (select-compiler
            $scope
            (select-ingredients-plus-the 
              $ingredients 
              (option-or
                (expressions-expression-option
                  (ingredients-expressions
                    (compile-ingredients $scope $syntax-list)))
                (error "the must have single expression")))))
        (else 
          (error "select-compiler, expected not or the"))))
    (error "select compiler, expected not or the")))
