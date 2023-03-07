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
  leo/compiler/expressions
  leo/compiler/sexp-utils
  leo/compiler/expression
  leo/compiler/expression-resolve
  leo/compiler/expressions-utils
  leo/compiler/syntax-utils
  leo/compiler/syntax-expression
  leo/compiler/compiler-plus-expressions
  leo/compiler/expression-utils
  leo/compiler/package-utils
  leo/compiler/package-sexp
  leo/compiler/package
  leo/compiler/type
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
    (compiler-syntax-resolve-doing $compiler $syntax)
    (compiler-syntax-resolve-default $compiler $syntax)))

(define (compiler-syntax-resolve-do
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (syntax-symbol-match-args $syntax `do $do-syntax-list
    (define $scope (compiler-scope $compiler))
    (define $package (compiler-package $compiler))
    (compiler $scope
      (package
        (package-do $package
          (lambda (($scope : Scope))
            (scope-syntax-list-expressions 
              (push-stack (compiler-scope $compiler) $scope) 
              $do-syntax-list)))))))

(define (compiler-syntax-resolve-doing
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (syntax-symbol-match-args $syntax `doing $doing-syntax-list
    (define $scope (compiler-scope $compiler))
    (define $package (compiler-package $compiler))
    (compiler $scope
      (package
        (package-do $package
          (lambda (($scope : Scope))
            (scope-syntax-list-expressions 
              (push-stack (compiler-scope $compiler) $scope) 
              $doing-syntax-list)))))))

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
          (expression-expressions (field-expression $syntax-e)))
        ((list? $syntax-e)
          (define $car (syntax-e (car $syntax-e)))
          (unless (symbol? $car) (error "parse-error not symbol"))
          (define $symbol $car)
          (define $syntax-list (cdr $syntax-e))
          (define $package (scope-syntax-list-package $scope $syntax-list))
          (define $structure (package-structure $package))
          (or
            (option-bind (structure-lift $structure) $structure-a
              (expression-expressions 
                (type-expression (field $symbol $structure-a))))
            (symbol-package-expressions $symbol $package)))
        (else (error "parse error unknown"))))))

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
    (scope-syntax-list-expressions null-scope (list #`number)))
  (pair null-sexp (structure (a number-type))))
  
(check-equal?
  (expressions-sexp-structure
    (scope-syntax-list-expressions null-scope (list #`(big number))))
  (pair null-sexp (structure (a (field `big (structure number-type))))))
  
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
