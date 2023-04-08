#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expression-utils
  leo/compiler/ingredients-utils
  leo/compiler/sexp-expression
  leo/compiler/ingredients
  leo/compiler/syntax-utils
  leo/compiler/type)

(define compile-ingredients-parameter : (Parameterof (-> Scope (Listof Syntax) Ingredients))
  (make-parameter
    (lambda (($scope : Scope) ($syntax-list : (Listof Syntax))) : Ingredients
      (ingredients
        (expressions
          (make-syntax `(compiled ,@$syntax-list))
          (scope-structure $scope))))))

(define (compile-ingredients-recursively
  ($scope : Scope)
  ($syntax-list : (Listof Syntax))) : Ingredients
  ((compile-ingredients-parameter) $scope $syntax-list))

(define (compile-expressions-recursively
  ($scope : Scope)
  ($syntax-list : (Listof Syntax))) : Expressions
  (ingredients-expressions
    (compile-ingredients-recursively $scope $syntax-list)))
