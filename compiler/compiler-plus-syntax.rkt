#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing
  leo/compiler/compiler
  leo/compiler/base-scope
  leo/compiler/scope
  leo/compiler/package
  leo/compiler/expression
  leo/compiler/package-utils
  leo/compiler/syntax-expression
  leo/compiler/compiler-plus-expression
  leo/compiler/expression-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define (scope-syntax-list-package 
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Package
  (tuple-package
    (compiler-tuple
      (fold 
        (compiler $scope null-tuple)
        $syntax-list
        compiler-plus-syntax))))

(define (compiler-plus-syntax 
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : Compiler
  (compiler-plus-expression
    $compiler
    (scope-syntax-expression 
      (compiler-scope $compiler)
      $syntax)))

(define (scope-syntax-expression 
  ($scope : Scope) 
  ($syntax : Syntax)) 
  : Expression
  (or 
    (syntax-expression-option $syntax)
    (let ()
      (define $syntax-e (syntax-e $syntax))
      (cond
        ((null? $syntax-e) (error "parse error null"))
        ((symbol? $syntax-e)
          (field-expression $syntax-e))
        ((list? $syntax-e)
          (define $car (syntax-e (car $syntax-e)))
          (unless (symbol? $car) (error "parse-error not symbol"))
          (define $symbol $car)
          (define $syntax-list (cdr $syntax-e))
          (define $package (scope-syntax-list-package $scope $syntax-list))
          (symbol-package-expression $symbol $package))
        (else (error "parse error unknown"))))))
  
(check-equal?
  (package-sexp-structure
    (scope-syntax-list-package
      base-scope
      (list
        #`(int 1) 
        #`(plus (int 2))
        #`text)))
  (pair 
    `(number->string (unsafe-fx+ 1 2)) 
    (structure text-type)))
