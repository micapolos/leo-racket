#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/option
  leo/typed/testing
  leo/compiler/package
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/expressions-sexp
  leo/compiler/expressions-utils)

(define (package-sexp ($package : Package)) : Sexp
  `(package
    ,@(reverse 
      (map expressions-sexp $package))))

(check-equal?
  (package-sexp
    (package expressions-ab expressions-cd))
  `(package
    (expressions ab (structure (racket a) (racket b)))
    (expressions cd (structure (racket c) (racket d)))))
