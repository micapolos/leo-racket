#lang leo/typed

(require
  leo/compiler/binding
  leo/compiler/compiler
  leo/compiler/switch
  leo/compiler/generate-temporary
  leo/compiler/expressions
  leo/compiler/expression
  leo/compiler/syntax-utils
  leo/compiler/expression-resolve
  leo/compiler/expressions-utils
  leo/compiler/syntax-type
  leo/compiler/syntax-utils
  leo/compiler/syntax-expression
  leo/compiler/compile-recursively
  leo/compiler/compiler-plus-expressions
  leo/compiler/expression-utils
  leo/compiler/compiler-utils
  leo/compiler/ingredients-utils
  leo/compiler/ingredients-sexp
  leo/compiler/ingredients
  leo/compiler/ingredients-action
  leo/compiler/ingredients-action-compiler
  leo/compiler/match-compiler
  leo/compiler/select-compiler
  leo/compiler/select-ingredients
  leo/compiler/recipe-compiler
  leo/compiler/repeat-compiler
  leo/compiler/type
  leo/compiler/type-utils)

(define (compiler-plus-syntax ($compiler : Compiler) ($syntax : Syntax)) : Compiler
  (or
    (syntax-match-symbol-args (field-syntax $syntax) $symbol $syntax-list
      (case $symbol
        ((a) (compiler-apply-a $compiler $syntax-list))
        ((everything) (compiler-apply-everything $compiler $syntax-list))
        ((apply) (compiler-apply-apply $compiler $syntax-list))
        ((debug) (compiler-apply-debug $compiler $syntax-list))
        ((do in) (compiler-apply-do $compiler $syntax-list))
        ((it the take) (compiler-apply-it $compiler $syntax-list))
        ((package) (compiler-apply-package $compiler $syntax-list))
        ((quote) (compiler-apply-quote $compiler $syntax-list))
        ((racket) (compiler-apply-racket $compiler $syntax-list))
        ((recipe) (compiler-apply-recipe $compiler $syntax-list))
        ((repeat) (compiler-apply-repeat $compiler $syntax-list))
        ((choice) (compiler-apply-select $compiler $syntax-list))
        ((switch) (compiler-apply-switch $compiler $syntax-list))
        ((time) (compiler-apply-time $compiler $syntax-list))
        ((then) (compiler-apply-then $compiler $syntax-list))
        ((thing) (compiler-apply-thing $compiler $syntax-list))
        ((type) (compiler-apply-type $compiler $syntax-list))
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
    (scope-resolve-first-fn $scope
      (lambda (($binding : Binding))
        ($fn (binding-expression $binding))))))

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
  (define $ingredients (compile-ingredients-recursively $scope $syntax-list))
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
          (ingredients-expressions
            (compile-ingredients-recursively
              (push-stack (compiler-scope $compiler) $scope)
              $syntax-list)))))))

(define (compiler-apply-repeat
  ($compiler : Compiler)
  ($syntax-list : (Listof Syntax))) : Compiler
  (define $ingredients (compiler-ingredients $compiler))
  (compiler-with-ingredients $compiler
    (ingredients
      (or
        (expression-apply-expressions
          (compile-repeat-expression
            (compiler-scope $compiler)
            (ingredients-structure $ingredients)
            $syntax-list)
          (ingredients-expressions $ingredients))
        (error "apply error")))))

(define (compiler-apply-it 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-ingredients $compiler
    (ingredients-plus
      (compiler-ingredients $compiler)
      (compile-ingredients-recursively
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
            (compile-ingredients-recursively
              (compiler-scope $compiler)
              $syntax-list)))
        (expressions
          (make-syntax `(time ,(expressions-syntax $expressions)))
          (expressions-structure $expressions))))))

(define (compiler-apply-then 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-ingredients $compiler
    (ingredients
      (ingredients-do (compiler-ingredients $compiler)
        (lambda (($scope : Scope))
          (ingredients-expressions
            (ingredients-plus
              (map expression-expressions (map binding-expression $scope))
              (compile-ingredients-recursively
                (push-stack (compiler-scope $compiler) $scope)
                $syntax-list))))))))

(define (compiler-apply-thing
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax)))
: Compiler
  (define $syntax 
    (option-or (single $syntax-list)
      (error "top syntax error")))
  (define $index (syntax-e $syntax))
  (unless (exact-positive-integer? $index)
    (error "thing: syntax error, positive integer expected"))
  (compiler-thing $compiler $index))

(define (compiler-apply-everything 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax)))
: Compiler
  (unless (null? $syntax-list)
    (error "everything: syntax error"))
  (compiler-everything $compiler))

; ----------------------------------------------------------------------------

(define (compiler-apply-ingredients-action ($compiler : Compiler) ($action : Ingredients-Action)) : Compiler
  (compiler-with-ingredients $compiler
    (ingredients-apply-action
      (compiler-ingredients $compiler) $action)))

(define (compiler-apply-a 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-apply-ingredients-action
    $compiler
    (compile-ingredients-action-a $compiler $syntax-list)))

(define (compiler-apply-recipe 
  ($compiler : Compiler) 
  ($syntax-list : (Listof Syntax))) 
  : Compiler
  (compiler-with-ingredients $compiler
    (push-stack
      (compiler-ingredients $compiler)
      (scope-syntax-list-arrow-ingredients
        (compiler-scope $compiler)
        $syntax-list))))

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
        (compiler null-tuple 
          (ingredients 
            (expression-expressions 
              (number-expression 3.14))))
        #`b)))
  (ingredients-sexp
    (ingredients
      (expressions #`3.14 (structure number-type))
      (expressions null-syntax (structure static-type-b)))))

; number plus field
(check-equal?
  (ingredients-sexp
    (compiler-ingredients
      (compiler-plus-syntax
        (compiler null-tuple 
          (ingredients 
            (expression-expressions 
              (number-expression 3.14))))
        #`foo)))
  (ingredients-sexp
    (ingredients
      (expressions #`3.14 (structure number-type))
      (expressions null-syntax (structure (field! `foo))))))

; number plus string
(check-equal?
  (ingredients-sexp
    (compiler-ingredients
      (compiler-plus-syntax
        (compiler null-tuple 
          (ingredients
            (expression-expressions 
              (number-expression 3.14))))
        #`"foo")))
  (ingredients-sexp
    (ingredients
      (expressions #`3.14 (structure number-type))
      (expressions #`"foo" (structure text-type)))))
