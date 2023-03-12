#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/typed/syntax-match
  leo/compiler/package
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
  leo/compiler/type-match
  leo/compiler/compile-package)

(data recipe-compiler 
  (scope : Scope) 
  (package : Recipe-Package))

(define (null-recipe-compiler ($scope : Scope))
  (recipe-compiler $scope null-recipe-package))

(define (recipe-compiler-package-stack ($compiler : Recipe-Compiler)) : (Stackof Recipe-Package)
  (stack (recipe-compiler-package $compiler)))

(define (scope-syntax-list-arrow-package
  ($scope : Scope) 
  ($syntax-list : (Listof Syntax))) 
  : Package
  (map recipe-package-arrow-expressions
    (recipe-compiler-package-stack
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
  (define $arrow-expressions-option (recipe-package-arrow-expressions-option $recipe-package))
  (when $arrow-expressions-option (error "Recipe already have a body"))
  (define $lhs-structure (recipe-package-lhs-structure $recipe-package))
  (define $rhs-structure-option (recipe-package-rhs-structure-option $recipe-package))
  (or
    (syntax-symbol-match-args $syntax `giving $syntax-list
      (when $rhs-structure-option (error "Recipe already has giving"))
      (struct-copy recipe-compiler $recipe-compiler
        (package
          (struct-copy recipe-package $recipe-package
            (rhs-structure-option (syntax-list-structure $syntax-list))))))
    (syntax-symbol-match-args $syntax `does $syntax-list
      (bind $lhs-scope (structure-generate-scope $lhs-structure)
        (struct-copy recipe-compiler $recipe-compiler
          (package
            (struct-copy recipe-package $recipe-package
              (arrow-expressions-option
                (let ()
                  (define $expressions 
                    (scope-doing-expressions
                      $lhs-scope
                      (package-expressions
                        (compile-package
                          (push-stack $scope $lhs-scope) 
                          $syntax-list))))
                  (when 
                    (and 
                      $rhs-structure-option 
                      (structure-matches? 
                        (expressions-structure $expressions) 
                        $rhs-structure-option))
                    (error "recipe giving doing type mismatch"))
                  $expressions)))))))
    (let ()
      (when $rhs-structure-option (error "recipe expected does"))
      (struct-copy recipe-compiler $recipe-compiler
        (package 
          (struct-copy recipe-package $recipe-package
            (lhs-structure 
              (push 
                $lhs-structure 
                (syntax-type $syntax)))))))))

; ---------------------------------------------------------------------

(check-equal?
  (map expressions-sexp-structure
    (scope-syntax-list-arrow-package
      base-scope
      (syntax-e #`(number increment (does number (plus 1))))))
  (stack
    (pair
      `(lambda (tmp-number) recurse)
      (structure 
        (arrow 
          (structure number-type (field! `increment))
          (structure (racket)))))))
