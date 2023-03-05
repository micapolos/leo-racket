#lang typed/racket/base

(require 
  leo/typed/option
  leo/typed/stack
  leo/compiler/binding
  leo/compiler/binding-resolve
  leo/compiler/scope
  leo/compiler/scope-resolve
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-resolve
  leo/compiler/expression-utils
  leo/compiler/package-utils)

(scope-resolve-tuple
  (scope 
    (binding
      (arrow 
        (structure text-type (field `plus (structure text-type)))
        (structure text-type))
      `string-append)
    (binding
      (arrow 
        (structure int-type (field `text null-structure))
        (structure text-type))
      `number->string)
    (binding
      (arrow 
        (structure int-type (field `plus (structure int-type)))
        (structure int-type))
      `unsafe-fx+)
    (binding
      (arrow 
        (structure int-type (field `times (structure int-type)))
        (structure int-type))
      `unsafe-fx*))
  (tuple
    (int-expression 1)
    (field-expression `text null-structure)))
