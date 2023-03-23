#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expression-utils
  leo/compiler/ingredients-utils
  leo/compiler/sexp-expression
  leo/compiler/ingredients
  leo/compiler/syntax-utils
  leo/compiler/type)

(define compile-ingredients-parameter : (Parameterof (-> Tuple (Listof Syntax) Ingredients))
  (make-parameter
    (lambda (($tuple : Tuple) ($syntax-list : (Listof Syntax))) : Ingredients
      (ingredients
        (expressions
          (make-syntax `(compiled ,@$syntax-list))
          (tuple-structure $tuple))))))

(define (compile-ingredients
  ($tuple : Tuple) 
  ($syntax-list : (Listof Syntax))) : Ingredients
  ((compile-ingredients-parameter) $tuple $syntax-list))

(define (compile-expressions
  ($tuple : Tuple)
  ($syntax-list : (Listof Syntax))) : Expressions
  (ingredients-expressions
    (compile-ingredients $tuple $syntax-list)))
