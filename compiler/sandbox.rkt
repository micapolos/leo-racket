#lang typed/racket/base

(require 
  leo/typed/option
  leo/typed/stack
  leo/compiler/expression-utils
  leo/compiler/package-utils)

(package-unsafe-ref
  (option-unsafe-ref
    (expression-field-rhs
      (field-expression `point
        (expression-stack-package
          (stack
            (text-expression "foo")
            (number-expression 123)
            (boolean-expression #f))))))
  0)