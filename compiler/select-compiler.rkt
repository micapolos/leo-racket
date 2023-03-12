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
  leo/compiler/compile-expressions-part
  leo/compiler/expression-utils
  leo/compiler/expressions-utils
  leo/compiler/expressions-part-utils
  leo/compiler/select-expressions-part)

(data select-compiler
  (scope : Scope)
  (expressions-part : Select-Expressions-Part))

(define (compile-select-expressions-part
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Select-Expressions-Part
  (select-compiler-expressions-part
    (fold
      (select-compiler $scope null-select-expressions-part)
      $syntax-list
      select-compiler-plus-syntax)))

(define (select-compiler-plus-syntax
  ($compiler : Select-Compiler)
  ($syntax : Syntax))
  : Select-Compiler
  (define $scope (select-compiler-scope $compiler))
  (define $expressions-part (select-compiler-expressions-part $compiler))
  (or
    (syntax-match-symbol-args $syntax $symbol $syntax-list
      (cond
        ((equal? $symbol `not)
          (select-compiler
            $scope
            (select-expressions-part-plus-not 
              $expressions-part
              (option-ref-or 
                (single (syntax-list-structure $syntax-list))
                (error "not must have single type")))))
        ((equal? $symbol `the)
          (select-compiler
            $scope
            (select-expressions-part-plus-the 
              $expressions-part 
              (option-ref-or
                (expressions-expression-option
                  (expressions-part-expressions
                    (compile-expressions-part $scope $syntax-list)))
                (error "the must have single expression")))))
        (else 
          (error "select-compiler, expected not or the"))))
    (error "select compiler, expected not or the")))
