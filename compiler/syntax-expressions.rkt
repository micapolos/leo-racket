#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing
  leo/compiler/expressions
  leo/compiler/base-scope
  leo/compiler/scope-utils
  leo/compiler/syntax-utils
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/ingredients
  leo/compiler/compiler-plus-syntax)

(define (syntax-list-ingredients ($syntax-list : (Listof Syntax))) : Ingredients
  (tuple-syntax-list-ingredients (scope-tuple base-scope) $syntax-list))

(define (syntax-list-expressions ($syntax-list : (Listof Syntax))) : Expressions
  (tuple-syntax-list-expressions (scope-tuple base-scope) $syntax-list))

(define (sexp-expressions ($sexp : Sexp)) : Expressions
  (define $datum (sexp-datum $sexp))
  (syntax-list-expressions
    (cond
      ((list? $datum) (map make-syntax $datum))
      (else (list (make-syntax $datum))))))

(check-equal?
  (expressions-sexp
    (sexp-expressions `(1 (plus 2) text)))
  `(expressions
    (let-values (((tmp-number) (#%app + 1 2)))
      (#%app number->string tmp-number))
    (structure text)))
