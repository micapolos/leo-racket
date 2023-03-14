#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/expressions
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-sexp)

(define (expressions-sexp ($expressions : Expressions)) : Sexp
  `(expressions 
    ,(syntax->datum (expressions-syntax $expressions))
    ,(structure-sexp (expressions-structure $expressions))))

(check-equal?
  (expressions-sexp
    (expressions syntax-a (structure static-type-b static-type-c)))
  `(expressions (dynamic-a) (structure b c)))
