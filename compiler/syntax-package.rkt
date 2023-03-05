#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/package
  leo/compiler/base-scope
  leo/compiler/syntax-utils
  leo/compiler/package-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/compiler-plus-syntax)

(define (syntax-list-package ($syntax-list : (Listof Syntax))) : Package
  (scope-syntax-list-package base-scope $syntax-list))

(define (sexp-package ($sexp : Sexp)) : Package
  (define $datum (sexp-datum $sexp))
  (syntax-list-package
    (cond
      ((list? $datum) (map make-syntax $datum))
      (else (list (make-syntax $datum))))))

(check-equal?
  (package-sexp-structure
    (sexp-package `((int 1) (plus (int 2)) text)))
  (pair `(number->string (unsafe-fx+ 1 2)) (structure text-type)))
