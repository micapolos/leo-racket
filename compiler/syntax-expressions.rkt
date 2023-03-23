#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/expressions
  leo/compiler/base-tuple
  leo/compiler/syntax-utils
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/ingredients
  leo/compiler/compiler-plus-syntax
  leo/compiler/compile)

(define (syntax-list-ingredients ($syntax-list : (Listof Syntax))) : Ingredients
  (compile-ingredients base-tuple $syntax-list))

(define (syntax-list-expressions ($syntax-list : (Listof Syntax))) : Expressions
  (compile-expressions base-tuple $syntax-list))

(define (sexp-expressions ($sexp : Sexp)) : Expressions
  (define $datum (sexp-datum $sexp))
  (syntax-list-expressions
    (cond
      ((list? $datum) (map make-syntax $datum))
      (else (list (make-syntax $datum))))))

(check-equal?
  (expressions-sexp
    (sexp-expressions `(1 (plus 2) text)))
  `(expressions (number->string (+ 1 2)) (structure text)))
