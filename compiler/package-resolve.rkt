#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/option
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/package
  leo/compiler/package-utils
  leo/compiler/expressions
  leo/compiler/expressions-utils)

(define (package-resolve-the
  ($package : Package)
  ($expressions : Expressions))
  : (Option Package)
  (option-app push
    $package
    (expressions-symbol-rhs $expressions `the)))
  