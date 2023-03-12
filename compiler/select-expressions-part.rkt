#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/select-expression-utils
  leo/compiler/type)

(data select-expressions-part
  (selection : (Option (Pairof Exact-Nonnegative-Integer Syntax)))
  (structure : Structure))

(define null-select-expressions-part 
  (select-expressions-part #f null-structure))

(define (select-expressions-part-plus-not 
  ($expressions-part : Select-Expressions-Part)
  ($type : Type)) : Select-Expressions-Part
  (select-expressions-part
    (select-expressions-part-selection $expressions-part)
    (push
      (select-expressions-part-structure $expressions-part)
      $type)))

(define (select-expressions-part-plus-the 
  ($expressions-part : Select-Expressions-Part)
  ($expression : Expression)) : Select-Expressions-Part
  (unless (not (select-expressions-part-selection $expressions-part))
    (error "already selected"))
  (select-expressions-part
    (pair 
      (length (select-expressions-part-structure $expressions-part))
      (expression-syntax $expression))
    (push
      (select-expressions-part-structure $expressions-part)
      (expression-type $expression))))

(define (select-expressions-part-expression
  ($expressions-part : Select-Expressions-Part)) : Expression
  (define $selection (select-expressions-part-selection $expressions-part))
  (unless $selection (error "not selected"))
  (index-syntax-structure-select-expression 
    (car $selection)
    (cdr $selection)
    (select-expressions-part-structure $expressions-part)))
