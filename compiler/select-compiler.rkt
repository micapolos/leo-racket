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
  leo/compiler/compile-package
  leo/compiler/expression-utils
  leo/compiler/expressions-utils
  leo/compiler/package-utils
  leo/compiler/select-package)

(data select-compiler
  (scope : Scope)
  (package : Select-Package))

(define (compile-select-package
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Select-Package
  (select-compiler-package
    (fold
      (select-compiler $scope null-select-package)
      $syntax-list
      select-compiler-plus-syntax)))

(define (select-compiler-plus-syntax
  ($compiler : Select-Compiler)
  ($syntax : Syntax))
  : Select-Compiler
  (define $scope (select-compiler-scope $compiler))
  (define $package (select-compiler-package $compiler))
  (or
    (syntax-match-symbol-args $syntax $symbol $syntax-list
      (cond
        ((equal? $symbol `not)
          (select-compiler
            $scope
            (select-package-plus-not 
              $package
              (option-ref-or 
                (single (syntax-list-structure $syntax-list))
                (error "not must have single type")))))
        ((equal? $symbol `the)
          (select-compiler
            $scope
            (select-package-plus-the 
              $package 
              (option-ref-or
                (expressions-expression-option
                  (package-expressions
                    (compile-package $scope $syntax-list)))
                (error "the must have single expression")))))
        (else 
          (error "select-compiler, expected not or the"))))
    (error "select compiler, expected not or the")))
