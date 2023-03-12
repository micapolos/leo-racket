#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/option
  leo/typed/testing
  leo/compiler/expressions-part
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/expressions-sexp
  leo/compiler/expressions-utils)

(define (expressions-part-sexp ($expressions-part : Expressions-Part)) : Sexp
  `(expressions-part
    ,@(reverse 
      (map expressions-sexp $expressions-part))))

(check-equal?
  (expressions-part-sexp
    (expressions-part expressions-ab expressions-cd))
  `(expressions-part
    (expressions ab (structure (a racket) (b racket)))
    (expressions cd (structure (c racket) (d racket)))))
