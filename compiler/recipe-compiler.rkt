#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/ingredients
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/ingredients-utils
  leo/compiler/ingredients-sexp
  leo/compiler/base-tuple
  leo/compiler/recipe-part
  leo/compiler/syntax-type
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-match
  leo/compiler/repeat-compiler
  leo/compiler/compile-recursively)

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
    (syntax-match-symbol-args $syntax $symbol $syntax-list
      (case $symbol
        ((does gives)
          (bind $lhs-scope (structure-generate-scope $lhs-structure)
            (struct-copy recipe-compiler $recipe-compiler
              (ingredients
                (push $ingredients
                  (let ()
                    (define $expressions
                      (scope-does-expressions
                        $lhs-scope
                        (ingredients-expressions
                          (compile-ingredients-recursively
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
        (else #f)))
    (syntax-match-symbol-args $syntax $symbol $syntax-list
      (case $symbol
        ((repeats)
          (struct-copy recipe-compiler $recipe-compiler
            (ingredients
              (push $ingredients
                (expression-expressions
                  (compile-repeat-expression
                    $scope
                    $lhs-structure
                    $syntax-list))))
            (recipe-part null-recipe-part)))
        (else #f)))
    (let ()
      (when $rhs-structure-option (error "recipe expected does / gives"))
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
    (scope-syntax-list-arrow-ingredients
      null-scope
      (syntax-e #`(number (does text)))))
  (ingredients-sexp
    (ingredients
      (expressions
        #`(lambda (tmp-number) (compiled text))
        (structure (recipe! number-type (doing number-type)))))))
