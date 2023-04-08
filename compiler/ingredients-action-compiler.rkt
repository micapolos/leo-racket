#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/compiler
  leo/compiler/action
  leo/compiler/ingredients
  leo/compiler/ingredients-action
  leo/compiler/ingredients-utils
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/compile-recursively
  leo/compiler/syntax-type)

(define (compile-ingredients-action-a ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Ingredients-Action
  (append-action
    (ingredients
      (expression-expressions
        (reified-expression
          (syntax-list-structure $syntax-list))))))

(define (compile-ingredients-action-do ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Ingredients-Action
  (set-action
    (ingredients
      (ingredients-do (compiler-ingredients $compiler)
        (lambda (($scope : Scope))
          (ingredients-expressions
            (compile-ingredients-recursively
              (push-stack (compiler-scope $compiler) $scope)
              $syntax-list)))))))
