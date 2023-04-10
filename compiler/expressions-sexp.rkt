#lang leo/typed

(require
  leo/compiler/expressions
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-sexp)

(define (expressions-sexp ($expressions : Expressions)) : Sexp
  `(expressions 
    ,(option-app syntax->datum (expressions-syntax-option $expressions))
    ,(structure-sexp (expressions-structure $expressions))))

(check-equal?
  (expressions-sexp
    (expressions syntax-a (structure static-type-b static-type-c)))
  `(expressions a (structure b c)))
