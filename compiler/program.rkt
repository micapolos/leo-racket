#lang leo/typed

(require
  leo/compiler/binder
  leo/compiler/expressions
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/ingredients-sexp)

(data program
  (entry-stack : (Stackof Entry))
  (body-ingredients : Ingredients))

(define null-program
  (program null null-ingredients))

(define (program-sexp ($program : Program)) : Sexp
  `(program
    (stack ,@(reverse (map entry-sexp (program-entry-stack $program))))
    ,(ingredients-sexp (program-body-ingredients $program))))

(define (program-ingredients ($program : Program)) : Ingredients
  (define $entry-stack (program-entry-stack $program))
  (define $ingredients (program-body-ingredients $program))
  (cond
    ((null? $entry-stack) $ingredients)
    (else
      (define $expressions (ingredients-expressions $ingredients))
      (ingredients
        (expressions
          (option-app entry-stack-do-syntax $entry-stack (expressions-syntax-option $expressions))
          (expressions-structure $expressions))))))
