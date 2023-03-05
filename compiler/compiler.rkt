#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/scope
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expression-resolve
  leo/compiler/package-utils
  leo/compiler/scope-resolve
  leo/compiler/type-utils)

(data compiler 
  (scope : Scope) 
  (tuple : Tuple))

(define null-compiler (compiler null-scope null-tuple))

(define (compiler-plus-expression 
  ($compiler : Compiler) 
  ($expression : Expression)) : Compiler
  (define $scope (compiler-scope $compiler))
  (define $tuple (push (compiler-tuple $compiler) $expression))
  (option-app compiler
    $scope
    (or
      (option-app package-tuple
        (or
          (scope-resolve-tuple $scope $tuple)
          (tuple-resolve $tuple)))
      $tuple)))

(check-equal?
  (map expression-sexp-type
    (compiler-tuple
      (compiler-plus-expression null-compiler
        (number-expression 3.14))))
  (stack (pair 3.14 number-type)))
