#lang leo/typed

(require
  leo/compiler/expressions
  leo/compiler/expression)

(define-type Line
  (U
    Expressions
    Do-Line
    Apply-Line))

(data do-line (expressions : Expressions))
(data apply-line (expressions : Expressions))
