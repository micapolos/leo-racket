#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/expressions
  leo/compiler/syntax-expressions
  leo/compiler/syntax-utils
  leo/compiler/ingredients
  leo/compiler/module-syntax
  leo/typed/syntax-match)

(define (leo-compile ($sexp-list : (Listof Sexp))) : (Pairof Sexp Structure)
  (expressions-sexp-structure
    (syntax-list-expressions
      (map make-syntax
        (map sexp-datum $sexp-list)))))

(define (leo-compile-any ($any : Any)) : Syntax
  (expressions-syntax 
    (syntax-list-expressions 
      (syntax-syntax-list (any-syntax $any)))))

(define (leo-compile-any-list ($any-list : (Listof Any))) : (Listof Syntax)
  (reverse
    (ingredients-top-level-syntax-stack
      (syntax-list-ingredients
        (map syntax-normalize (map any-syntax $any-list))))))
