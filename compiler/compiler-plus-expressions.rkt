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
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/package
  leo/compiler/package-utils
  leo/compiler/package-sexp
  leo/compiler/scope-resolve
  leo/compiler/type
  leo/compiler/type-utils)

(define (compiler-plus-quoted-tuple
  ($compiler : Compiler) 
  ($tuple : Tuple)) : Compiler
  (struct-copy compiler $compiler
    (package 
      (package-plus-tuple 
        (compiler-package $compiler) 
        $tuple))))

(define (compiler-plus-expressions
  ($compiler : Compiler) 
  ($expressions : Expressions)) : Compiler
  (define $scope (compiler-scope $compiler))
  (define $package 
    (push (compiler-package $compiler) $expressions))
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
      (compiler-plus-expressions
        (compiler null-scope (package expressions-a))
        expressions-b)))
  `(package 
    (expressions a (structure (a racket)))
    (expressions b (structure (b racket)))))

(check-equal?
  (package-sexp
    (compiler-package
      (compiler-plus-expressions
        (compiler
          (scope
            (binding
              (arrow
                (structure text-type (field `plus (structure text-type)))
                (structure text-type))
              #`string-append))
          (package (expression-expressions (text-expression "Hello, "))))
        (expression-expressions 
          (field-expression `plus 
            (tuple (text-expression "world!")))))))
  `(package
    (expressions
      (string-append "Hello, " "world!") 
      (structure text))))
