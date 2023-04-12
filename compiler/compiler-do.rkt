#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/compiler
  leo/compiler/compiler-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/expressions
  leo/compiler/ingredients
  leo/compiler/ingredients-utils)

(define (compiler-do ($compiler : Compiler) ($fn : (-> Compiler Compiler))) : Compiler
  (compiler-with-ingredients $compiler
    (ingredients
      (ingredients-do (compiler-ingredients $compiler)
        (lambda (($scope : Scope))
          (ingredients-expressions
            (compiler-ingredients
              ($fn
                (compiler
                  (push-stack (compiler-scope $compiler) $scope)
                  null-ingredients)))))))))

(check-equal?
  (compiler-sexp
    (compiler-do
      (compiler
        (scope (binding identifier-a dynamic-type-a))
        (ingredients (expressions syntax-b (structure dynamic-type-b))))
      (lambda (($compiler : Compiler))
        (check-equal?
          (compiler-sexp $compiler)
          (compiler-sexp
            (compiler
              (scope
                (binding identifier-a dynamic-type-a)
                (binding #`tmp-b dynamic-type-b))
              null-ingredients)))
        (compiler
          (scope (binding identifier-c dynamic-type-c))
          (ingredients (expressions syntax-d (structure dynamic-type-d)))))))
  (compiler-sexp
    (compiler
      (scope (binding identifier-a dynamic-type-a))
      (ingredients
        (expressions
          (make-syntax `(let-values (((tmp-b) b)) d))
          (structure dynamic-type-d))))))
