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
  leo/compiler/compile-expressions-part
  leo/compiler/compiler-plus-expressions
  leo/compiler/expression-utils
  leo/compiler/compiler-utils
  leo/compiler/expressions-part-utils
  leo/compiler/expressions-part-sexp
  leo/compiler/expressions-part
  leo/compiler/select-compiler
  leo/compiler/select-expressions-part
  leo/compiler/recipe-compiler
  leo/compiler/recipe-part
  leo/compiler/type
  leo/compiler/type-sexp
  leo/compiler/type-utils)

(define (scope-syntax-list-expressions-part 
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Expressions-Part
  (parameterize ((compile-expressions-part-parameter scope-syntax-list-expressions-part))
    (compiler-expressions-part
      (fold 
        (compiler $scope null-tuple)
        $syntax-list
        compiler-plus-syntax))))

(define (scope-syntax-list-expressions 
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Expressions
  (expressions-part-expressions
    (scope-syntax-list-expressions-part
      $scope $syntax-list)))

(define (compiler-plus-syntax 
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : Compiler
  (or
    (compiler-syntax-resolve-a $compiler $syntax)
    (compiler-syntax-resolve-do $compiler $syntax)
    (compiler-syntax-resolve-recipe $compiler $syntax)
    (compiler-syntax-resolve-quote $compiler $syntax)
    (compiler-syntax-resolve-apply $compiler $syntax)
    (compiler-syntax-resolve-racket $compiler $syntax)
    (compiler-syntax-resolve-the $compiler $syntax)
    (compiler-syntax-resolve-then $compiler $syntax)
    (compiler-syntax-resolve-time $compiler $syntax)
    (compiler-syntax-resolve-type $compiler $syntax)
    (compiler-syntax-resolve-select $compiler $syntax)
    (compiler-syntax-resolve-default $compiler $syntax)))

(define (compiler-syntax-resolve-a
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (syntax-symbol-match-args $syntax `a $syntax-list
    (compiler-apply-a $compiler $syntax-list)))

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

(define (compiler-syntax-resolve-select
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (syntax-symbol-match-args $syntax `select $syntax-list
    (parameterize ((compile-expressions-part-parameter scope-syntax-list-expressions-part))
      (compiler-apply-select $compiler $syntax-list))))

(define (compiler-syntax-resolve-quote
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (syntax-symbol-match-args $syntax `quote $syntax-list
    (compiler-apply-quote $compiler $syntax-list)))

(define (compiler-syntax-resolve-apply
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (and (equal? (syntax->datum $syntax) `apply)
    (compiler-apply-apply $compiler)))

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

(define (compiler-syntax-resolve-time
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : (Option Compiler)
  (and (equal? (syntax->datum $syntax) `time)
    (compiler-apply-time $compiler)))

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
  (define $expressions-part (scope-syntax-list-expressions-part $scope $syntax-list))
  (define $structure (expressions-part-structure $expressions-part))
  (symbol-expressions-part-expressions $symbol $expressions-part))

; ------------------------------------------------------------------------------------

(define (compiler-apply-do 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-expressions-part $compiler
    (expressions-part
      (expressions-part-do (compiler-expressions-part $compiler)
        (lambda (($scope : Scope))
          (scope-syntax-list-expressions 
            (push-stack (compiler-scope $compiler) $scope) 
            $syntax-list))))))

(define (compiler-apply-the 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-expressions-part $compiler
    (expressions-part-plus
      (compiler-expressions-part $compiler)
      (scope-syntax-list-expressions-part
        (compiler-scope $compiler)
        $syntax-list))))

(define (compiler-apply-time ($compiler : Compiler)) : Compiler
  (compiler-with-expressions-part $compiler
    (expressions-part
      (bind $expressions
        (expressions-part-expressions (compiler-expressions-part $compiler))
        (expressions
          (make-syntax `(time ,(expressions-syntax $expressions)))
          (expressions-structure $expressions))))))

(define (compiler-apply-then 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-expressions-part $compiler
    (expressions-part-plus (compiler-expressions-part $compiler)
      (expressions-part
        (expressions-part-do (compiler-expressions-part $compiler)
          (lambda (($scope : Scope))
            (scope-syntax-list-expressions 
              (push-stack (compiler-scope $compiler) $scope) 
              $syntax-list)))))))

; ----------------------------------------------------------------------------

(define (compiler-apply-a 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-expressions-part $compiler
    (push-stack
      (compiler-expressions-part $compiler)
      (map expression-expressions 
        (map type-expression 
          (syntax-list-structure $syntax-list))))))

(define (compiler-apply-recipe 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-expressions-part $compiler
    (push-stack
      (compiler-expressions-part $compiler)
      (parameterize ((compile-expressions-part-parameter scope-syntax-list-expressions-part))
        (scope-syntax-list-arrow-expressions-part
          (compiler-scope $compiler)
          $syntax-list)))))

(define (compiler-apply-select 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-expressions-part $compiler
    (push
      (compiler-expressions-part $compiler)
      (expression-expressions
        (select-expressions-part-expression
          (compile-select-expressions-part
            (compiler-scope $compiler)
            $syntax-list))))))

; -------------------------------------------------------------------------------------

; number plus static
(check-equal?
  (expressions-part-sexp
    (compiler-expressions-part
      (compiler-plus-syntax
        (compiler null-scope 
          (expressions-part 
            (expression-expressions 
              (number-expression 3.14))))
        #`b)))
  `(expressions-part
    (expressions 3.14 (structure number))
    (expressions #f (structure b))))

; number plus field
(check-equal?
  (expressions-part-sexp
    (compiler-expressions-part
      (compiler-plus-syntax
        (compiler null-scope 
          (expressions-part 
            (expression-expressions 
              (number-expression 3.14))))
        #`foo)))
  `(expressions-part
    (expressions 3.14 (structure number))
    (expressions #f (structure foo))))

; number plus string
(check-equal?
  (expressions-part-sexp
    (compiler-expressions-part
      (compiler-plus-syntax
        (compiler null-scope 
          (expressions-part
            (expression-expressions 
              (number-expression 3.14))))
        #`"foo")))
  `(expressions-part
    (expressions 3.14 (structure number))
    (expressions "foo" (structure text))))

(check-equal?
  (expressions-sexp-structure
    (scope-syntax-list-expressions
      base-scope
      (list
        #`1
        #`(plus 2)
        #`text)))
  (pair 
    `(#%app number->string (#%app + 1 2)) 
    (structure text-type)))

(check-equal?
  (expressions-sexp-structure
    (scope-syntax-list-expressions
      base-scope
      (list
        #`1
        #`(dodać 2)
        #`(do number (add dodać number)))))
  (pair 
    `(let-values (((tmp-number) 1) ((tmp-dodać) 2)) 
      (#%app + tmp-number tmp-dodać))
    (structure number-type)))
