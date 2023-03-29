#lang leo/typed

(require
  leo/compiler/binder
  leo/compiler/ingredients
  leo/compiler/ingredients-sexp)

(data program
  (entry-stack : (Stackof Entry))
  (ingredients : Ingredients))

(define null-program
  (program null null-ingredients))

(define (program-sexp ($program : Program)) : Sexp
  `(program
    (stack ,@(reverse (map entry-sexp (program-entry-stack $program))))
    ,(ingredients-sexp (program-ingredients $program))))
