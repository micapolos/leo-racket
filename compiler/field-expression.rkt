#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expression-stack-syntax
  leo/compiler/package
  leo/compiler/package-utils
  leo/compiler/type
  leo/compiler/typed
  leo/compiler/type-utils)

(define (symbol-package-expression 
  ($symbol : Symbol)
  ($package : Package)) 
  : Expression
  (expression
    (package-syntax $package)
    (field $symbol (package-structure $package))))

(define (field-expression 
  ($symbol : Symbol) 
  ($expression-stack : (Stackof Expression)))
  : Expression
  (symbol-package-expression
    $symbol
    (expression-stack-package $expression-stack)))

(check-equal?
  (expression-typed-datum
    (field-expression `point
      (stack 
        dynamic-expression-a 
        static-expression-b 
        dynamic-expression-c)))
  (typed 
    `(cons a c) 
    (field `point 
      (stack 
        dynamic-type-a 
        static-type-b 
        dynamic-type-c))))
