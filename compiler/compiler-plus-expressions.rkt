#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
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
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/ingredients-sexp
  leo/compiler/scope-resolve
  leo/compiler/type
  leo/compiler/type-utils)

(define (compiler-plus-quoted-tuple
  ($compiler : Compiler) 
  ($tuple : Tuple)) : Compiler
  (struct-copy compiler $compiler
    (ingredients 
      (ingredients-plus-tuple 
        (compiler-ingredients $compiler) 
        $tuple))))

(define (compiler-plus-expressions
  ($compiler : Compiler) 
  ($expressions : Expressions)) : Compiler
  (struct-copy compiler $compiler
    (ingredients 
      (scope-apply-ingredients 
        (compiler-scope $compiler)
        (push (compiler-ingredients $compiler) $expressions)))))

(define (scope-apply-ingredients
  ($scope : Scope) 
  ($ingredients : Ingredients)) : Ingredients
  (or
    (option-app ingredients
      (ingredients-resolve-fn $ingredients
        (curry scope-or-tuple-resolve-tuple $scope)))
    $ingredients))

(define (scope-or-tuple-resolve-tuple
  ($scope : Scope) 
  ($tuple : Tuple)) : (Option Expressions)
  (or
    (scope-resolve-tuple $scope $tuple)
    (tuple-resolve $tuple)))

(check-equal?
  (ingredients-sexp
    (compiler-ingredients
      (compiler-plus-expressions
        (compiler null-scope (ingredients expressions-a))
        expressions-b)))
  `(ingredients 
    (expressions a (structure (a racket)))
    (expressions b (structure (b racket)))))

(check-equal?
  (ingredients-sexp
    (compiler-ingredients
      (compiler-plus-expressions
        (compiler
          (scope
            (binding
              (arrow
                (structure text-type (field `plus (structure text-type)))
                (structure text-type))
              #`string-append))
          (ingredients (expression-expressions (text-expression "Hello, "))))
        (expression-expressions 
          (field-expression `plus 
            (tuple (text-expression "world!")))))))
  `(ingredients
    (expressions
      (#%app string-append "Hello, " "world!") 
      (structure text))))
