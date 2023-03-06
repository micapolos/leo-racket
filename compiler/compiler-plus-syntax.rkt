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
  leo/compiler/compiler-plus-expression
  leo/compiler/expression-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define (scope-syntax-list-expressions 
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Expressions
  (tuple-expressions
    (compiler-tuple
      (fold 
        (compiler $scope null-tuple)
        $syntax-list
        compiler-plus-syntax))))

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
    (define $tuple (compiler-tuple $compiler))
    (compiler $scope
      (expressions-tuple
        (tuple-do $tuple
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
    (define $tuple (compiler-tuple $compiler))
    (option-app compiler $scope
      (option-app expressions-tuple
        (tuple-doing $tuple
          (lambda (($scope : Scope))
            (scope-syntax-list-expressions 
              (push-stack (compiler-scope $compiler) $scope) 
              $doing-syntax-list)))))))

(define (compiler-syntax-resolve-default
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
          (define $expressions (scope-syntax-list-expressions $scope $syntax-list))
          (define $structure (expressions-structure $expressions))
          (or
            (option-bind (structure-lift $structure) $structure-a
              (type-expression (field $symbol $structure-a)))
            (symbol-expressions-expression $symbol $expressions)))
        (else (error "parse error unknown"))))))

; ----------------------------------------------------------------------------

(check-equal?
  (map expression-sexp-type
    (compiler-tuple
      (compiler-plus-syntax
        (compiler null-scope (tuple (number-expression 3.14)))
        #`b)))
  (stack 
    (pair 3.14 number-type)
    (pair null-sexp static-type-b)))

(check-equal?
  (map expression-sexp-type
    (compiler-tuple
      (compiler-plus-syntax
        (compiler null-scope (tuple (number-expression 3.14)))
        #`foo)))
  (stack 
    (pair 3.14 number-type)
    (pair #f (field `foo null-structure))))

(check-equal?
  (map expression-sexp-type
    (compiler-tuple
      (compiler-plus-syntax
        (compiler null-scope (tuple (number-expression 3.14)))
        #`"foo")))
  (stack 
    (pair 3.14 number-type)
    (pair "foo" text-type)))

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
    `(let ((tmp-int (unsafe-fx+ 1 2))) (unsafe-fx+ tmp-int tmp-int))
    (structure int-type)))
