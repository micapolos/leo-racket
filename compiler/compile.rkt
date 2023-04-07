#lang leo/typed

(require
  leo/compiler/expression
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
  leo/compiler/syntax-utils)

(define (compile-ingredients
  ($tuple : Tuple)
  ($syntax-list : (Listof Syntax)))
  : Ingredients
  (parameterize ((compile-ingredients-parameter compile-ingredients))
    (program-compiler-ingredients
      (fold
        (program-compiler $tuple null-program)
        $syntax-list
        program-compiler-plus-syntax))))

(define (compile-expressions
  ($tuple : Tuple)
  ($syntax-list : (Listof Syntax)))
  : Expressions
  (ingredients-expressions
    (compile-ingredients
      $tuple $syntax-list)))

; ------------------------------------------------------------------------------

(check-equal?
  (syntax->datum
    (expression-syntax
      (parameterize ((compile-ingredients-parameter compile-ingredients))
        (compile-repeat-expression
          null-tuple
          null-structure
          (list
            (make-syntax `text)
            (make-syntax `(doing "foo")))))))
  `(letrec ((tmp-recipe (lambda () "foo"))) tmp-recipe))
