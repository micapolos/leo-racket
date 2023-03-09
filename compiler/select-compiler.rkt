#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/syntax-match
  leo/compiler/scope
  leo/compiler/syntax-type
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
  ($select-compiler : Select-Compiler)
  ($syntax : Syntax))
  : Select-Compiler
  (or
    (syntax-match-symbol-args $syntax $symbol $args
      (case $symbol
        ((not) (syntax-type $syntax))
        ((the) (error "TODO"))
        (else (error "select-compiler, expected not or the"))))
    (error "select compiler, expected not or the")))

