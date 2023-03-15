#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/typed/syntax-match
  leo/compiler/ingredients
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/ingredients-utils
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
  (scope : Scope) 
  (ingredients : Ingredients)
  (recipe-part : Recipe-Part))

(define (null-recipe-compiler ($scope : Scope))
  (recipe-compiler $scope null null-recipe-part))

(define (scope-syntax-list-arrow-ingredients
  ($scope : Scope) 
  ($syntax-list : (Listof Syntax))) 
  : Ingredients
  (recipe-compiler-ingredients
    (fold
      (null-recipe-compiler $scope)
      $syntax-list
      recipe-compiler-plus-syntax)))

(define (recipe-compiler-plus-syntax 
  ($recipe-compiler : Recipe-Compiler) 
  ($syntax : Syntax))
  : Recipe-Compiler
  (define $scope (recipe-compiler-scope $recipe-compiler))
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
      (bind $lhs-scope (structure-generate-scope $lhs-structure)
        (struct-copy recipe-compiler $recipe-compiler
          (ingredients
            (push $ingredients
              (let ()
                (define $expressions 
                  (scope-doing-expressions
                    $lhs-scope
                    (ingredients-expressions
                      (compile-ingredients
                        (push-stack $scope $lhs-scope) 
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
  (map expressions-sexp-structure
    (scope-syntax-list-arrow-ingredients
      base-scope
      (syntax-e #`(number increment (does number (plus 1))))))
  (stack
    (pair
      `(lambda (tmp-number) recurse)
      (structure 
        (arrow 
          (structure number-type (field! `increment))
          (structure (racket)))))))
