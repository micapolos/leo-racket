#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/typed/syntax-match
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/package-utils
  leo/compiler/base-scope
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/recipe-package
  leo/compiler/syntax-type
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/compile-package)

(data recipe-compiler 
  (scope : Scope) 
  (package : Recipe-Package))

(define (null-recipe-compiler ($scope : Scope))
  (recipe-compiler $scope null-recipe-package))

(define (scope-syntax-list-arrow-expressions
  ($scope : Scope) 
  ($syntax-list : (Listof Syntax))) 
  : Expressions
  (recipe-package-arrow-expressions
    (recipe-compiler-package
      (fold
        (null-recipe-compiler $scope)
        $syntax-list
        recipe-compiler-plus-syntax))))

(define (recipe-compiler-plus-syntax 
  ($recipe-compiler : Recipe-Compiler) 
  ($syntax : Syntax)) 
  : Recipe-Compiler
  (define $scope (recipe-compiler-scope $recipe-compiler))
  (define $recipe-package (recipe-compiler-package $recipe-compiler))
  (when (recipe-package-arrow-expressions-option $recipe-package)
    (error "Recipe already have a body"))
  (define $lhs-structure (recipe-package-lhs-structure $recipe-package))
  (or
    (syntax-symbol-match-args $syntax `does $syntax-list
      (bind $lhs-scope (structure-generate-scope $lhs-structure)
        (struct-copy recipe-compiler $recipe-compiler
          (package
            (struct-copy recipe-package $recipe-package
              (arrow-expressions-option
                (scope-doing-expressions
                  $lhs-scope
                  (package-expressions
                    (compile-package
                      (push-stack $scope $lhs-scope) 
                      $syntax-list)))))))))
    (struct-copy recipe-compiler $recipe-compiler
      (package 
        (struct-copy recipe-package $recipe-package
          (lhs-structure 
            (push 
              $lhs-structure 
              (syntax-type $syntax))))))))

; ---------------------------------------------------------------------

(check-equal?
  (expressions-sexp-structure
    (scope-syntax-list-arrow-expressions
      base-scope
      (syntax-e #`(number increment (does number (plus 1))))))
  (pair
    `(lambda (tmp-number) recurse)
    (structure 
      (arrow 
        (structure number-type (null-field `increment))
        (structure (racket))))))
