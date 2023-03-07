#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/compiler
  leo/compiler/scope
  leo/compiler/binding
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expression-resolve
  leo/compiler/expressions-utils
  leo/compiler/package
  leo/compiler/package-utils
  leo/compiler/package-sexp
  leo/compiler/scope-resolve
  leo/compiler/type
  leo/compiler/type-utils)

(define (compiler-plus-expression 
  ($compiler : Compiler) 
  ($expression : Expression)) : Compiler
  (define $scope (compiler-scope $compiler))
  (define $package 
    (push 
      (compiler-package $compiler) 
      (expression-expressions $expression)))
  (option-app compiler
    $scope
    (or
      (option-app package
        (package-resolve-fn $package
          (lambda (($tuple : Tuple))
            (or
              (scope-resolve-tuple $scope $tuple)
              (tuple-resolve $tuple)))))
      $package)))

(check-equal?
  (package-sexp
    (compiler-package
      (compiler-plus-expression
        (compiler null-scope (package expressions-a))
        expression-b)))
  `(package 
    (expressions a (structure (racket a)))
    (expressions b (structure (racket b)))))

(check-equal?
  (package-sexp
    (compiler-package
      (compiler-plus-expression
        (compiler
          (scope
            (binding
              (arrow
                (structure text-type (field `plus (structure text-type)))
                (structure text-type))
              #`string-append))
          (package (expression-expressions (text-expression "Hello, "))))
        (field-expression `plus 
          (tuple (text-expression "world!"))))))
  `(package
    (expressions
      (string-append "Hello, " "world!") 
      (structure (text (racket string))))))
