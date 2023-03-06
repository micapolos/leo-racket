#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/package
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-sexp)

(define (package-sexp ($package : Package)) : Sexp
  `(package 
    ,(syntax->datum (package-syntax $package))
    ,(structure-sexp (package-structure $package))))

(check-equal?
  (package-sexp
    (package syntax-a (structure static-type-b static-type-c)))
  `(package a (structure b c)))
