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
  (cond
    ((expressions? $expressions-part)
      `(expressions-part ,(expressions-sexp $expressions-part)))
    (else 
      `(expressions-part
        ,@(reverse (map expression-sexp $expressions-part))))))

(check-equal?
  (expressions-part-sexp expressions-ab)
  `(expressions-part
    (expressions ab (structure (a racket) (b racket)))))

(check-equal?
  (expressions-part-sexp (tuple expression-a expression-b))
  `(expressions-part
    (expression (dynamic-a) (a racket))
    (expression (dynamic-b) (b racket))))
