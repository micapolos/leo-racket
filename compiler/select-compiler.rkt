#lang leo/typed

(require
  leo/compiler/syntax-type
  leo/compiler/expression
  leo/compiler/type
  leo/compiler/compile-recursively
  leo/compiler/expression-utils
  leo/compiler/expressions-utils
  leo/compiler/ingredients-utils
  leo/compiler/select-ingredients)

(data select-compiler
  (tuple : Tuple)
  (ingredients : Select-Ingredients))

(define (compile-select-ingredients
  ($tuple : Tuple)
  ($syntax-list : (Listof Syntax)))
  : Select-Ingredients
  (select-compiler-ingredients
    (fold
      (select-compiler $tuple null-select-ingredients)
      $syntax-list
      select-compiler-plus-syntax)))

(define (select-compiler-plus-syntax
  ($compiler : Select-Compiler)
  ($syntax : Syntax))
  : Select-Compiler
  (define $tuple (select-compiler-tuple $compiler))
  (define $ingredients (select-compiler-ingredients $compiler))
  (or
    (syntax-match-symbol-args $syntax $symbol $syntax-list
      (cond
        ((equal? $symbol `not)
          (select-compiler
            $tuple
            (select-ingredients-plus-not 
              $ingredients
              (option-or 
                (single (syntax-list-structure $syntax-list))
                (error "not must have single type")))))
        ((equal? $symbol `the)
          (select-compiler
            $tuple
            (select-ingredients-plus-the 
              $ingredients 
              (option-or
                (expressions-expression-option
                  (ingredients-expressions
                    (compile-ingredients-recursively $tuple $syntax-list)))
                (error "the must have single expression")))))
        (else 
          (error "select-compiler, expected not or the"))))
    (error "select compiler, expected not or the")))
