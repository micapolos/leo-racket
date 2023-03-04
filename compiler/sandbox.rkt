#lang typed/racket/base

(require 
  leo/typed/option
  leo/typed/stack
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-resolve
  leo/compiler/expression-utils
  leo/compiler/package-utils)

(expression-resolve-tuple
  (expression #`(lambda ($number) (number->string $number))
    (arrow 
      (structure number-type (field `string null))
      (structure text-type)))
  (tuple
    (number-expression 3.14)
    (field-expression `string null-package)))
