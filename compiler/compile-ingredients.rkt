#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/sexp-expression
  leo/compiler/ingredients
  leo/compiler/type
  leo/compiler/scope)

(define compile-ingredients-parameter : (Parameterof (-> Scope (Listof Syntax) Ingredients))
  (make-parameter
    (lambda (($scope : Scope) ($syntax-list : (Listof Syntax))) : Ingredients
      (ingredients
        (expression-expressions
          (sexp-expression
            `(compiled
              ,(scope-sexp $scope)
              (script
                ,@(map syntax->datum $syntax-list)))))))))

(define (compile-ingredients
  ($scope : Scope) 
  ($syntax-list : (Listof Syntax))) : Ingredients
  ((compile-ingredients-parameter) $scope $syntax-list))

(define (recursive-compile-ingredients
  ($recurse : (-> Scope (Listof Syntax) Ingredients))
  ($scope : Scope) 
  ($syntax-list : (Listof Syntax))) : Ingredients
  (parameterize ((compile-ingredients-parameter $recurse))
    (compile-ingredients $scope $syntax-list)))
