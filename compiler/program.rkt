#lang leo/typed

(require
  leo/compiler/binder
  leo/compiler/expressions
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
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

(define (program-resolved-ingredients ($program : Program)) : Ingredients
  (define $entry-stack (program-entry-stack $program))
  (define $ingredients (program-ingredients $program))
  (cond
    ((null? $entry-stack) $ingredients)
    (else
      (define $expressions (ingredients-expressions $ingredients))
      (ingredients
        (expressions
          (entry-stack-do-syntax $entry-stack (expressions-syntax $expressions))
          (expressions-structure $expressions))))))
