#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/option
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/typed/syntax-match
  leo/compiler/compiler
  leo/compiler/base-scope
  leo/compiler/scope
  leo/compiler/sexp-expression
  leo/compiler/expressions
  leo/compiler/scope-utils
  leo/compiler/sexp-utils
  leo/compiler/expression
  leo/compiler/expression-resolve
  leo/compiler/expressions-utils
  leo/compiler/syntax-type
  leo/compiler/syntax-utils
  leo/compiler/syntax-expression
  leo/compiler/compiler-plus-expressions
  leo/compiler/expression-utils
  leo/compiler/compiler-utils
  leo/compiler/package-utils
  leo/compiler/package-sexp
  leo/compiler/package
  leo/compiler/recipe-compiler
  leo/compiler/recipe-package
  leo/compiler/type
  leo/compiler/type-type
  leo/compiler/type-sexp
  leo/compiler/type-utils)

(define (scope-syntax-list-package 
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Package
  (compiler-package
    (fold 
      (compiler $scope null-tuple)
      $syntax-list
      compiler-plus-syntax)))

(define (scope-syntax-list-expressions 
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Expressions
  (package-expressions
    (scope-syntax-list-package 
      $scope $syntax-list)))

(define (compiler-plus-syntax 
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : Compiler
  (or
    (compiler-syntax-resolve-do $compiler $syntax)
    (compiler-syntax-resolve-recipe $compiler $syntax)
    (compiler-syntax-resolve-quote $compiler $syntax)
    (compiler-syntax-resolve-racket $compiler $syntax)
    (compiler-syntax-resolve-the $compiler $syntax)
    (compiler-syntax-resolve-then $compiler $syntax)
    (compiler-syntax-resolve-type $compiler $syntax)
    (compiler-syntax-resolve-default $compiler $syntax)))

(define (compiler-syntax-resolve-do
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (syntax-symbol-match-args $syntax `do $syntax-list
    (compiler-apply-do $compiler $syntax-list)))

(define (compiler-syntax-resolve-recipe
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (syntax-symbol-match-args $syntax `recipe $syntax-list
    (compiler-apply-recipe $compiler $syntax-list)))

(define (compiler-syntax-resolve-quote
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (syntax-symbol-match-args $syntax `quote $syntax-list
    (compiler-apply-quote $compiler $syntax-list)))

(define (compiler-syntax-resolve-racket
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (and (equal? (syntax->datum $syntax) `racket)
    (compiler-apply-racket $compiler)))

(define (compiler-syntax-resolve-the
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (syntax-symbol-match-args $syntax `the $syntax-list
    (compiler-apply-the $compiler $syntax-list)))

(define (compiler-syntax-resolve-then
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (syntax-symbol-match-args $syntax `then $syntax-list
    (compiler-apply-then $compiler $syntax-list)))

(define (compiler-syntax-resolve-type
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (and (equal? (syntax->datum $syntax) `type)
    (compiler-apply-type $compiler)))

(define (compiler-syntax-resolve-default
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : Compiler
  (compiler-plus-expressions
    $compiler
    (scope-syntax-expressions
      (compiler-scope $compiler)
      $syntax)))

(define (scope-syntax-expressions
  ($scope : Scope) 
  ($syntax : Syntax)) 
  : Expressions
  (or 
    (option-app expression-expressions
      (syntax-expression-option $syntax))
    (let ()
      (define $syntax-e (syntax-e $syntax))
      (cond
        ((null? $syntax-e) (error "parse error null"))
        ((symbol? $syntax-e)
          (scope-symbol-syntax-list-expressions $scope $syntax-e null))
        ((list? $syntax-e)
          (define $car (syntax-e (car $syntax-e)))
          (unless (symbol? $car) (error "parse-error not symbol"))
          (scope-symbol-syntax-list-expressions $scope $car (cdr $syntax-e)))
        (else (error "parse error unknown"))))))

(define (scope-symbol-syntax-list-expressions
  ($scope : Scope) 
  ($symbol : Symbol)
  ($syntax-list : (Listof Syntax)))
  : Expressions
  (define $package (scope-syntax-list-package $scope $syntax-list))
  (define $structure (package-structure $package))
  (symbol-package-expressions $symbol $package))

; ------------------------------------------------------------------------------------

(define (compiler-apply-do 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-package $compiler
    (package
      (package-do (compiler-package $compiler)
        (lambda (($scope : Scope))
          (scope-syntax-list-expressions 
            (push-stack (compiler-scope $compiler) $scope) 
            $syntax-list))))))

(define (compiler-apply-the 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-package $compiler
    (package-plus
      (compiler-package $compiler)
      (scope-syntax-list-package
        (compiler-scope $compiler)
        $syntax-list))))

(define (compiler-apply-then 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-package $compiler
    (package-plus (compiler-package $compiler)
      (package
        (package-do (compiler-package $compiler)
          (lambda (($scope : Scope))
            (scope-syntax-list-expressions 
              (push-stack (compiler-scope $compiler) $scope) 
              $syntax-list)))))))

; ----------------------------------------------------------------------------

(define (compiler-apply-recipe 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-package $compiler
    (push
      (compiler-package $compiler)
      (scope-syntax-list-arrow-expressions
        (compiler-scope $compiler)
        $syntax-list))))

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
                  (scope-syntax-list-expressions
                    (push-stack $scope $lhs-scope) 
                    $syntax-list))))))))
    (struct-copy recipe-compiler $recipe-compiler
      (package 
        (struct-copy recipe-package $recipe-package
          (lhs-structure 
            (push 
              $lhs-structure 
              (syntax-type $syntax))))))))

; ----------------------------------------------------------------------------

; number plus static
(check-equal?
  (package-sexp
    (compiler-package
      (compiler-plus-syntax
        (compiler null-scope 
          (package 
            (expression-expressions 
              (number-expression 3.14))))
        #`b)))
  `(package
    (expressions 3.14 (structure number))
    (expressions #f (structure b))))

; number plus field
(check-equal?
  (package-sexp
    (compiler-package
      (compiler-plus-syntax
        (compiler null-scope 
          (package 
            (expression-expressions 
              (number-expression 3.14))))
        #`foo)))
  `(package
    (expressions 3.14 (structure number))
    (expressions #f (structure foo))))

; number plus string
(check-equal?
  (package-sexp
    (compiler-package
      (compiler-plus-syntax
        (compiler null-scope 
          (package
            (expression-expressions 
              (number-expression 3.14))))
        #`"foo")))
  `(package
    (expressions 3.14 (structure number))
    (expressions "foo" (structure text))))

(check-equal?
  (expressions-sexp-structure
    (scope-syntax-list-expressions
      base-scope
      (list
        #`(int 1) 
        #`(plus (int 2))
        #`text)))
  (pair 
    `(number->string (unsafe-fx+ 1 2)) 
    (structure text-type)))

(check-equal?
  (expressions-sexp-structure
    (scope-syntax-list-expressions
      base-scope
      (list
        #`(int 1)
        #`(plus (int 2))
        #`(do int (plus int)))))
  (pair 
    `(let-values (((tmp-int) (unsafe-fx+ 1 2))) 
      (unsafe-fx+ tmp-int tmp-int))
    (structure int-type)))

(check-equal?
  (expressions-sexp-structure
    (scope-syntax-list-arrow-expressions
      base-scope
      (syntax-e #`(number increment (does number (plus 1))))))
  (pair
    `(lambda (tmp-number) (+ tmp-number 1))
    (structure 
      (arrow 
        (structure number-type (null-field `increment))
        (structure number-type)))))
