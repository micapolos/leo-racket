#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/ingredients
  leo/compiler/ingredients-sexp)

(data compiler 
  (scope : Scope)
  (ingredients : Ingredients))

(define null-compiler (compiler null-scope null-ingredients))

(define (compiler-sexp ($compiler : Compiler)) : Sexp
  `(compiler
    ,(scope-sexp (compiler-scope $compiler))
    ,(ingredients-sexp (compiler-ingredients $compiler))))
