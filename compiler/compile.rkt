#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/compiler
  leo/compiler/program
  leo/compiler/program-compiler
  leo/compiler/compiler-plus-syntax
  leo/compiler/compile-recursively
  leo/compiler/repeat-compiler
  leo/compiler/type
  leo/compiler/syntax-utils
  leo/compiler/line)

(define (compile-program
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Program
  (parameterize ((compile-ingredients-parameter compile-ingredients))
    (program-compiler-program
      (fold
        (program-compiler $scope null-program)
        $syntax-list
        program-compiler-plus-syntax))))

(define (compile-ingredients
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Ingredients
  (parameterize ((compile-ingredients-parameter compile-ingredients))
    (program-compiler-ingredients
      (fold
        (program-compiler $scope null-program)
        $syntax-list
        program-compiler-plus-syntax))))

(define (compile-expressions
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Expressions
  (ingredients-expressions
    (compile-ingredients
      $scope $syntax-list)))

; ------------------------------------------------------------------------------

(check-equal?
  (option-app syntax->datum
    (expression-syntax-option
      (parameterize ((compile-ingredients-parameter compile-ingredients))
        (compile-repeat-expression
          null-tuple
          null-structure
          (list
            (make-syntax `text)
            (make-syntax `(doing "foo")))))))
  `(letrec ((tmp-recipe (lambda () "foo"))) tmp-recipe))

; ------------------------------------------------------------------------------

(define (compile-symbol-line ($scope : Scope) ($symbol : Symbol) ($syntax-list : (Listof Syntax))) : Line
  (case $symbol
    (else (compile-symbol-expressions $scope $symbol $syntax-list))))

(define (compile-symbol-expressions ($scope : Scope) ($symbol : Symbol) ($syntax-list : (Listof Syntax))) : Expressions
  (scope-symbol-syntax-list-expressions $scope $symbol $syntax-list))
