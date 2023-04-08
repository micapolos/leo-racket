#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/compiler
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expression-resolve
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/ingredients-sexp
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
      (or
        (usage-ingredients-resolve-fn 'direct $ingredients
          (curry scope-or-tuple-resolve-tuple $scope))))
    $ingredients))

(define (scope-or-tuple-resolve-tuple
  ($lhs-scope : Scope)
  ($rhs-tuple : Tuple)) : (Option Expressions)
  (or
    (scope-resolve-tuple $lhs-scope $rhs-tuple)
    (tuple-resolve $rhs-tuple)))

(check-equal?
  (ingredients-sexp
    (compiler-ingredients
      (compiler-plus-expressions
        (compiler null-tuple (ingredients expressions-a))
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
              #`string-append
              (arrow
                (structure text-type (field `plus (structure text-type)))
                (structure text-type))))
          (ingredients (expression-expressions (text-expression "Hello, "))))
        (expression-expressions 
          (field-expression `plus 
            (tuple (text-expression "world!")))))))
  (ingredients-sexp
    (ingredients
      (expressions
        #`(string-append "Hello, " "world!")
        (structure text-type)))))
