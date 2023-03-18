#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/typed/syntax-match
  leo/compiler/ingredients
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/ingredients-utils
  leo/compiler/ingredients-sexp
  leo/compiler/base-scope
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/recipe-part
  leo/compiler/syntax-type
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-match
  leo/compiler/compile-ingredients)

(data recipe-compiler 
  (tuple : Tuple) 
  (ingredients : Ingredients)
  (recipe-part : Recipe-Part))

(define (null-recipe-compiler ($tuple : Tuple))
  (recipe-compiler $tuple null null-recipe-part))

(define (tuple-syntax-list-arrow-ingredients
  ($tuple : Tuple) 
  ($syntax-list : (Listof Syntax))) 
  : Ingredients
  (recipe-compiler-ingredients
    (fold
      (null-recipe-compiler $tuple)
      $syntax-list
      recipe-compiler-plus-syntax)))

(define (recipe-compiler-plus-syntax 
  ($recipe-compiler : Recipe-Compiler) 
  ($syntax : Syntax))
  : Recipe-Compiler
  (define $tuple (recipe-compiler-tuple $recipe-compiler))
  (define $recipe-part (recipe-compiler-recipe-part $recipe-compiler))
  (define $ingredients (recipe-compiler-ingredients $recipe-compiler))
  (define $lhs-structure (recipe-part-lhs-structure $recipe-part))
  (define $rhs-structure-option (recipe-part-rhs-structure-option $recipe-part))
  (or
    (syntax-symbol-match-args $syntax `giving $syntax-list
      (when $rhs-structure-option (error "Recipe already has giving"))
      (struct-copy recipe-compiler $recipe-compiler
        (recipe-part
          (struct-copy recipe-part $recipe-part
            (rhs-structure-option (syntax-list-structure $syntax-list))))))
    (syntax-symbol-match-args $syntax `does $syntax-list
      (bind $lhs-tuple (scope-tuple (structure-generate-scope $lhs-structure))
        (struct-copy recipe-compiler $recipe-compiler
          (ingredients
            (push $ingredients
              (let ()
                (define $expressions 
                  (tuple-doing-expressions
                    $lhs-tuple
                    (ingredients-expressions
                      (compile-ingredients
                        (push-stack $tuple $lhs-tuple) 
                        $syntax-list))))
                (when 
                  (and 
                    $rhs-structure-option 
                    (structure-matches? 
                      (expressions-structure $expressions) 
                      $rhs-structure-option))
                  (error "recipe giving doing type mismatch"))
                $expressions)))
          (recipe-part null-recipe-part))))
    (let ()
      (when $rhs-structure-option (error "recipe expected does"))
      (struct-copy recipe-compiler $recipe-compiler
        (recipe-part 
          (struct-copy recipe-part $recipe-part
            (lhs-structure 
              (push 
                $lhs-structure 
                (syntax-type $syntax)))))))))

; ---------------------------------------------------------------------

(check-equal?
  (ingredients-sexp
    (tuple-syntax-list-arrow-ingredients
      null-tuple
      (syntax-e #`(number (does text)))))
  `(ingredients
   (expressions
    (lambda (tmp-number) (values))
    (structure
     (recipe
      number
      (doing
       (compiled (tuple (expression tmp-number number)) (script text))))))))