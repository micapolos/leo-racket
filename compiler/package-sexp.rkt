#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/compiler/package
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/expressions-sexp
  leo/compiler/expression-utils)

(define (package-sexp ($package : Package)) : Sexp
  `(package
    ,(option-app expressions-sexp (package-expressions-option $package))
    ,(tuple-sexp (package-tuple $package))))

(check-equal?
  (package-sexp
    (package #f (tuple expression-a expression-b)))
  `(package #f
    (tuple 
      (expression a (racket a)) 
      (expression b (racket b)))))

(check-equal?
  (package-sexp
    (package 
      (expressions #'exp (structure type-a type-b))
      (tuple expression-c expression-d)))
  `(package
    (expressions exp (structure (racket a) (racket b)))
    (tuple 
      (expression c (racket c)) 
      (expression d (racket d)))))
