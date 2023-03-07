#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/expressions
  leo/compiler/base-scope
  leo/compiler/syntax-utils
  leo/compiler/expressions-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/compiler-plus-syntax)

(define (syntax-list-expressions ($syntax-list : (Listof Syntax))) : Expressions
  (scope-syntax-list-expressions base-scope $syntax-list))

(define (sexp-expressions ($sexp : Sexp)) : Expressions
  (define $datum (sexp-datum $sexp))
  (syntax-list-expressions
    (cond
      ((list? $datum) (map make-syntax $datum))
      (else (list (make-syntax $datum))))))

(check-equal?
  (expressions-sexp-structure
    (sexp-expressions `((int 1) (plus (int 2)) text)))
  (pair `(number->string (unsafe-fx+ 1 2)) (structure text-type)))