#lang leo/typed

(require 
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/base-tuple
  leo/compiler/syntax-utils
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/ingredients
  leo/compiler/program
  leo/compiler/compiler-plus-syntax
  leo/compiler/compile)

(define (syntax-list-program ($syntax-list : (Listof Syntax))) : Program
  (compile-program null-tuple $syntax-list))

(define (syntax-list-ingredients ($syntax-list : (Listof Syntax))) : Ingredients
  (compile-ingredients null-tuple $syntax-list))

(define (syntax-list-expressions ($syntax-list : (Listof Syntax))) : Expressions
  (compile-expressions null-tuple $syntax-list))

(define (sexp-expressions ($sexp : Sexp)) : Expressions
  (define $datum (sexp-datum $sexp))
  (syntax-list-expressions
    (cond
      ((list? $datum) (map make-syntax $datum))
      (else (list (make-syntax $datum))))))
