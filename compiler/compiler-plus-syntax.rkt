#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/option
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/typed/syntax-match
  leo/compiler/compiler
  leo/compiler/binding
  leo/compiler/binding-utils
  leo/compiler/base-scope
  leo/compiler/scope
  leo/compiler/scope-resolve
  leo/compiler/switch
  leo/compiler/sexp-expression
  leo/compiler/generate-temporary
  leo/compiler/expressions
  leo/compiler/expressions-sexp
  leo/compiler/scope-utils
  leo/compiler/sexp-utils
  leo/compiler/expression
  leo/compiler/syntax-utils
  leo/compiler/expression-resolve
  leo/compiler/expressions-utils
  leo/compiler/expressions-resolve
  leo/compiler/syntax-type
  leo/compiler/syntax-utils
  leo/compiler/syntax-expression
  leo/compiler/compile-ingredients
  leo/compiler/compiler-plus-expressions
  leo/compiler/expression-utils
  leo/compiler/compiler-utils
  leo/compiler/ingredients-utils
  leo/compiler/ingredients-sexp
  leo/compiler/ingredients
  leo/compiler/match-compiler
  leo/compiler/select-compiler
  leo/compiler/select-ingredients
  leo/compiler/recipe-compiler
  leo/compiler/recipe-part
  leo/compiler/type
  leo/compiler/type-sexp
  leo/compiler/type-symbol
  leo/compiler/type-utils)

(define (scope-syntax-list-ingredients 
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Ingredients
  (parameterize ((compile-ingredients-parameter scope-syntax-list-ingredients))
    (compiler-ingredients
      (fold 
        (compiler $scope null-tuple)
        $syntax-list
        compiler-plus-syntax))))

(define (scope-syntax-list-expressions 
  ($scope : Scope)
  ($syntax-list : (Listof Syntax)))
  : Expressions
  (ingredients-expressions
    (scope-syntax-list-ingredients
      $scope $syntax-list)))

(define (compiler-plus-syntax ($compiler : Compiler) ($syntax : Syntax)) : Compiler
  (define $normalized-syntax 
    (cond
      ((symbol? (syntax-e $syntax)) (make-syntax `(,$syntax)))
      (else $syntax)))
  (or
    (syntax-match-symbol-args $normalized-syntax $symbol $syntax-list
      (case $symbol
        ((a) (compiler-apply-a $compiler $syntax-list))
        ((do) (compiler-apply-do $compiler $syntax-list))
        ((recipe) (compiler-apply-recipe $compiler $syntax-list))
        ((quote) (compiler-apply-quote $compiler $syntax-list))
        ((apply) (compiler-apply-apply $compiler $syntax-list))
        ((switch) (compiler-apply-switch $compiler $syntax-list))
        ((racket) (compiler-apply-racket $compiler $syntax-list))
        ((the) (compiler-apply-the $compiler $syntax-list))
        ((then) (compiler-apply-then $compiler $syntax-list))
        ((time) (compiler-apply-time $compiler $syntax-list))
        ((type) (compiler-apply-type $compiler $syntax-list))
        ((select) (compiler-apply-select $compiler $syntax-list))
        (else #f)))
    (compiler-apply-syntax $compiler $syntax)))

(define (compiler-resolve-first-fn 
  ($compiler : Compiler) 
  ($fn : (-> Expression (Option Expressions)))) 
  : (Option Expressions)
  (define $scope (compiler-scope $compiler))
  (define $ingredients (compiler-ingredients $compiler))
  (or
    (option-app tuple-resolve-first-fn
      (option-app expressions-rhs-option 
        (ingredients-expressions $ingredients))
      $fn)
    (scope-resolve-first-fn $scope $fn)))

(define (compiler-apply-syntax ($compiler : Compiler) ($syntax : Syntax)) : Compiler
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
  (define $ingredients (scope-syntax-list-ingredients $scope $syntax-list))
  (define $structure (ingredients-structure $ingredients))
  (symbol-ingredients-expressions $symbol $ingredients))

; ------------------------------------------------------------------------------------

(define (compiler-apply-do 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-ingredients $compiler
    (ingredients
      (ingredients-do (compiler-ingredients $compiler)
        (lambda (($scope : Scope))
          (scope-syntax-list-expressions 
            (push-stack (compiler-scope $compiler) $scope) 
            $syntax-list))))))

(define (compiler-apply-the 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-ingredients $compiler
    (ingredients-plus
      (compiler-ingredients $compiler)
      (scope-syntax-list-ingredients
        (compiler-scope $compiler)
        $syntax-list))))

(define (compiler-apply-switch 
  ($compiler : Compiler)
  ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-ingredients $compiler
    (ingredients
      (option-or
        (compiler-resolve-first-fn $compiler
          (lambda (($expression : Expression))
            (let (($type (expression-type $expression)))
              (and
                (choice? $type)
                (let ()
                  (define $syntax (expression-syntax $expression))
                  (define $choice-type-stack (choice-type-stack $type))
                  (define $dynamic? (structure-dynamic? $choice-type-stack))
                  (define $selector-syntax (if $dynamic? (make-syntax `(car ,$syntax)) $syntax))
                  (define $value-syntax (if $dynamic? (make-syntax `(cdr ,$syntax)) null-syntax))
                  (define $identifier-option (if $dynamic? (symbol-temporary `value) #f))
                  (define $switch
                    (match-compiler-switch
                      (fold
                        (match-compiler
                          (compiler-scope $compiler)
                          $identifier-option
                          null-switch-option
                          (reverse (choice-type-stack $type)))
                        $syntax-list
                        match-compiler-plus-syntax)))
                  (define $switch-body
                    (syntax-switch-syntax-stack
                          $selector-syntax
                          (switch-syntax-stack $switch)))
                  (define $switch-syntax
                    (or
                      (and $identifier-option 
                        (make-syntax `(let ((,$identifier-option ,$value-syntax)) ,$switch-body)))
                      $switch-body))
                  (expression-expressions
                    (expression
                      $switch-syntax
                      (switch-type $switch))))))))
        (error "no choice to resolve")))))

(define (compiler-apply-time ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-ingredients $compiler
    (ingredients
      (bind $expressions
        (ingredients-expressions 
          (ingredients-plus 
            (compiler-ingredients $compiler)
            (compile-ingredients
              (compiler-scope $compiler)
              $syntax-list)))
        (expressions
          (make-syntax `(time ,(expressions-syntax $expressions)))
          (expressions-structure $expressions))))))

(define (compiler-apply-then 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-ingredients $compiler
    (ingredients-plus (compiler-ingredients $compiler)
      (ingredients
        (ingredients-do (compiler-ingredients $compiler)
          (lambda (($scope : Scope))
            (scope-syntax-list-expressions 
              (push-stack (compiler-scope $compiler) $scope) 
              $syntax-list)))))))

; ----------------------------------------------------------------------------

(define (compiler-apply-a 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-ingredients $compiler
    (push-stack
      (compiler-ingredients $compiler)
      (map expression-expressions 
        (map type-expression 
          (syntax-list-structure $syntax-list))))))

(define (compiler-apply-recipe 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-ingredients $compiler
    (push-stack
      (compiler-ingredients $compiler)
      (parameterize ((compile-ingredients-parameter scope-syntax-list-ingredients))
        (scope-syntax-list-arrow-ingredients
          (compiler-scope $compiler)
          $syntax-list)))))

(define (compiler-apply-select 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-ingredients $compiler
    (push
      (compiler-ingredients $compiler)
      (expression-expressions
        (select-ingredients-expression
          (compile-select-ingredients
            (compiler-scope $compiler)
            $syntax-list))))))

; -------------------------------------------------------------------------------------

; number plus static
(check-equal?
  (ingredients-sexp
    (compiler-ingredients
      (compiler-plus-syntax
        (compiler null-scope 
          (ingredients 
            (expression-expressions 
              (number-expression 3.14))))
        #`b)))
  `(ingredients
    (expressions 3.14 (structure number))
    (expressions #f (structure b))))

; number plus field
(check-equal?
  (ingredients-sexp
    (compiler-ingredients
      (compiler-plus-syntax
        (compiler null-scope 
          (ingredients 
            (expression-expressions 
              (number-expression 3.14))))
        #`foo)))
  `(ingredients
    (expressions 3.14 (structure number))
    (expressions #f (structure foo))))

; number plus string
(check-equal?
  (ingredients-sexp
    (compiler-ingredients
      (compiler-plus-syntax
        (compiler null-scope 
          (ingredients
            (expression-expressions 
              (number-expression 3.14))))
        #`"foo")))
  `(ingredients
    (expressions 3.14 (structure number))
    (expressions "foo" (structure text))))

(check-equal?
  (expressions-sexp
    (scope-syntax-list-expressions
      base-scope
      (list
        #`1
        #`(plus 2)
        #`text)))
  `(expressions
    (let-values (((tmp-text) (let-values (((tmp-number) (#%app + 1 2)))
      (#%app number->string tmp-number))))
     tmp-text)
   (structure text)))

(check-equal?
  (expressions-sexp
    (scope-syntax-list-expressions
      base-scope
      (list
        #`1
        #`(dodać 2)
        #`(do number (add dodać number)))))
  `(expressions
   (let-values (((tmp-number)
                 (let-values (((tmp-number) 1) ((tmp-dodać) 2))
                   (let-values (((tmp-number) (#%app + tmp-number tmp-dodać)))
                     tmp-number))))
     tmp-number)
   (structure number)))