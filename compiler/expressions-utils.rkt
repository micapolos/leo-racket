#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  racket/list
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/typed/stack
  leo/compiler/binding-utils
  leo/compiler/sexp-utils
  leo/compiler/binding
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/expressions
  leo/compiler/expressions-sexp
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-match
  leo/compiler/typed
  leo/compiler/type-utils)

(define null-expressions (expressions null-syntax null-structure))

(define static-expressions-a (expressions null-syntax static-structure-a))

(define expressions-a (expressions #`a structure-a))
(define expressions-b (expressions #`b structure-b))

(define expressions-ab (expressions #`ab structure-ab))
(define expressions-cd (expressions #`cd structure-cd))

(define (make-expressions ($syntax : Syntax) ($structure : Structure)) : Expressions
  (expressions
    (or (and (structure-dynamic? $structure) $syntax) null-syntax)
    $structure))

(define (expressions-size ($expressions : Expressions)) : Exact-Nonnegative-Integer
  (length (expressions-structure $expressions)))

(define (expression-expressions ($expression : Expression)) : Expressions
  (tuple-expressions (stack $expression)))

(define (tuple-expressions ($tuple : Tuple)) : Expressions
  (expressions
    (or (tuple-values-syntax-option $tuple) null-syntax)
    (tuple-structure $tuple)))

(check-equal?
  (expressions-sexp-structure (tuple-expressions (tuple static-expression-a)))
  (pair null-sexp (structure static-type-a)))

(check-equal?
  (expressions-sexp-structure
    (tuple-expressions
      (tuple dynamic-expression-a static-expression-b)))
  (pair 
    `a 
    (structure dynamic-type-a static-type-b)))

(check-equal?
  (expressions-sexp-structure
    (tuple-expressions
      (tuple dynamic-expression-a static-expression-b dynamic-expression-c)))
  (pair 
    `(values a c) 
    (structure dynamic-type-a static-type-b dynamic-type-c)))

; -------------------------------------------------------------------

(define (expressions-expression-option ($expressions : Expressions)) : (Option Expression)
  (option-bind (single (expressions-structure $expressions)) $type
    (expression (expressions-syntax $expressions) $type)))

(define (expressions-symbol-rhs
  ($expressions : Expressions)
  ($symbol : Symbol))
  : (Option Expressions)
  (option-bind (expressions-expression-option $expressions) $expression
    (bind $type (expression-type $expression)
      (and (field? $type) (equal? (field-symbol $type) `the)
        (expressions 
          (expression-syntax $expression)
          (field-structure $type))))))

(define (expressions-rhs-option ($expressions : Expressions)) : (Option Tuple)
  (option-app expression-rhs-tuple-option
    (expressions-expression-option $expressions)))

(check-equal?
  (option-app map expression-sexp-type
    (expressions-rhs-option
      (expressions syntax-a 
        (structure 
          (field `foo
            (structure type-b type-c))))))
  (stack
    (pair `(unsafe-car a) type-b)
    (pair `(unsafe-cdr a) type-c)))

(check-equal?
  (expressions-rhs-option
    (expressions syntax-a 
      (structure 
        (field `foo null)
        (field `bar null))))
  #f)

(check-equal?
  (expressions-rhs-option
    (expressions syntax-a (structure (racket))))
  #f)

; --------------------------------------------------------------

(define (expression-apply-expressions
  ($lhs-expression : Expression)
  ($rhs-expressions : Expressions))
  : (Option Expressions)
  (option-app expressions
    (make-syntax
      `(call-with-values
        (lambda () ,(expressions-syntax $rhs-expressions))
        ,(expression-syntax $lhs-expression)))
    (type-apply-structure 
      (expression-type $lhs-expression)
      (expressions-structure $rhs-expressions))))

(check-equal?
  (option-app expressions-sexp-structure
    (expression-apply-expressions
      (expression #`fn (arrow (structure type-a) (structure type-b)))
      (expressions #`pkg (structure type-a))))
  (pair 
    `(call-with-values (lambda () pkg) fn)
    (structure type-b)))

(check-equal?
  (expression-apply-expressions
    (expression #`fn (arrow (structure type-a) (structure type-b)))
    (expressions #`pkg (structure type-b)))
  #f)

; ---------------------------------------------------------------

(define (expressions-do-expression ($expressions : Expressions) ($fn : (-> Scope Expression))) : Expression
  (define $syntax (expressions-syntax $expressions))
  (define $structure (expressions-structure $expressions))
  (define $scope (structure-generate-scope $structure))
  (define $fn-expression ($fn $scope))
  (define $fn-syntax (expression-syntax $fn-expression))
  (define $fn-type (expression-type $fn-expression))
  (define $tmp-stack (scope-identifier-stack $scope))
  (make-expression
    (make-syntax 
      (and (type-dynamic? $fn-type)
        (case (length $tmp-stack)
          ((0) $fn-syntax)
          ((1) 
            `(let
              ((,(car $tmp-stack) ,$syntax))
              ,$fn-syntax))
          (else 
            `(let-values 
              (((,@(reverse $tmp-stack)) ,$syntax))
              ,$fn-syntax)))))
    $fn-type))

(check-equal?
  (expression-sexp-type
    (expressions-do-expression
      (expressions #`x (structure static-type-a))
      (lambda (($scope : Scope))
        (expression
          (make-syntax `(list ,@(reverse (scope-identifier-stack $scope))))
          (field `foo (reverse (scope-structure $scope)))))))
  (pair #f (field `foo (structure static-type-a))))

(check-equal?
  (expression-sexp-type
    (expressions-do-expression
      (expressions #`x (structure dynamic-type-a static-type-b))
      (lambda (($scope : Scope))
        (expression
          (make-syntax `(list ,@(reverse (scope-identifier-stack $scope))))
          (field `foo (reverse (scope-structure $scope)))))))
  (pair 
    `(let ((tmp-a x)) (list tmp-a)) 
    (field `foo (structure static-type-b dynamic-type-a))))

(check-equal?
  (expression-sexp-type
    (expressions-do-expression
      (expressions #`x (structure dynamic-type-a static-type-b dynamic-type-c))
      (lambda (($scope : Scope))
        (expression
          (make-syntax `(list ,@(scope-identifier-stack $scope)))
          (field `foo (reverse (scope-structure $scope)))))))
  (pair 
    `(let-values (((tmp-a tmp-c) x)) (list tmp-c tmp-a)) 
    (field `foo (structure dynamic-type-c static-type-b dynamic-type-a))))

; --------------------------------------------------------------------

(define (expressions-resolve-fn
  ($expressions : Expressions) 
  ($fn : (-> Tuple (Option Expressions)))) : 
  (Option Expressions)
  (define $syntax (expressions-syntax $expressions))
  (define $structure (expressions-structure $expressions))
  (define $scope (structure-generate-scope $structure))
  (define $tmp-stack (scope-identifier-stack $scope))
  (define $fn-expressions ($fn (map binding-expression $scope)))
  (and $fn-expressions
    (make-expressions
      (case (length $tmp-stack)
        ((0) $syntax)
        ((1) 
          (make-syntax 
            `(let
              ((,(top $tmp-stack) ,$syntax))
              ,(expressions-syntax $fn-expressions))))
        (else 
          (make-syntax 
            `(let-values
              (((,@(reverse $tmp-stack)) ,$syntax))
              ,(expressions-syntax $fn-expressions)))))
      (expressions-structure $fn-expressions))))

(check-equal?
  (option-app expressions-sexp
    (expressions-resolve-fn (expressions #`a (structure static-type-a))
      (lambda (($tuple : Tuple))
        (expressions
          (make-syntax (reverse (map expression-syntax $tuple)))
          (tuple-structure $tuple)))))
  `(expressions #f (structure a)))

(check-equal?
  (option-app expressions-sexp
    (expressions-resolve-fn (expressions atomic-syntax-a (structure dynamic-type-a))
      (lambda (($tuple : Tuple))
        (expressions
          (make-syntax (reverse (map expression-syntax $tuple)))
          (tuple-structure $tuple)))))
  `(expressions (let ((tmp-a atomic-a)) (tmp-a)) (structure (a racket))))

(check-equal?
  (option-app expressions-sexp
    (expressions-resolve-fn (expressions complex-syntax-a (structure dynamic-type-a))
      (lambda (($tuple : Tuple))
        (expressions
          (make-syntax (reverse (map expression-syntax $tuple)))
          (tuple-structure $tuple)))))
  `(expressions (let ((tmp-a (complex-a))) (tmp-a)) (structure (a racket))))

(check-equal?
  (option-app expressions-sexp
    (expressions-resolve-fn expressions-ab
      (lambda (($tuple : Tuple))
        (expressions 
          (make-syntax (reverse (map expression-syntax $tuple)))
          (tuple-structure $tuple)))))
  `(expressions
    (let-values (((tmp-a tmp-b) ab)) (tmp-a tmp-b))
    (structure (a racket) (b racket))))

; ---------------------------------------------------------------------

(define (expressions-do ($expressions : Expressions) ($fn : (-> Scope Expressions))) : Expressions
  (define $syntax (expressions-syntax $expressions))
  (define $structure (expressions-structure $expressions))
  (define $scope (structure-generate-scope $structure))
  (define $fn-expressions ($fn $scope))
  (define $fn-syntax (expressions-syntax $fn-expressions))
  (define $fn-structure (expressions-structure $fn-expressions))
  (define $tmp-stack (scope-identifier-stack $scope))
  (make-expressions
    (make-syntax 
      (case (length $tmp-stack)
        ((0) $fn-syntax)
        ((1) 
          `(let
            ((,(car $tmp-stack) ,$syntax))
            ,$fn-syntax))
        (else 
          `(let-values 
            (((,@(reverse $tmp-stack)) ,$syntax))
            ,$fn-syntax))))
    $fn-structure))

(check-equal?
  (expressions-sexp-structure
    (expressions-do
      (make-expressions #`pkg (structure static-type-a))
      (lambda (($scope : Scope)) 
        (make-expressions 
          (make-syntax `(values ,@(scope-identifier-stack $scope)))
          (reverse (scope-structure $scope))))))
  (pair 
    #f
    (structure static-type-a)))

(check-equal?
  (expressions-sexp-structure
    (expressions-do
      (expressions #`pkg (structure dynamic-type-a static-type-b))
      (lambda (($scope : Scope)) 
        (expressions 
          (make-syntax `(values ,@(scope-identifier-stack $scope)))
          (reverse (scope-structure $scope))))))
  (pair 
    `(let ((tmp-a pkg)) (values tmp-a)) 
    (structure static-type-b dynamic-type-a)))

(check-equal?
  (expressions-sexp-structure
    (expressions-do
      (expressions #`pkg 
        (structure 
          dynamic-type-a
          static-type-b
          dynamic-type-c))
      (lambda (($scope : Scope)) 
        (expressions 
          (make-syntax `(values ,@(scope-identifier-stack $scope)))
          (reverse (scope-structure $scope))))))
  (pair 
    `(let-values (((tmp-a tmp-c) pkg)) (values tmp-c tmp-a)) 
    (structure dynamic-type-c static-type-b dynamic-type-a)))

; -------------------------------------------------------------------------

(define (expressions-lift-structure ($expressions : Expressions)) : (Option Structure)
  (structure-lift (expressions-structure $expressions)))

; ---------------------------------------------------------

(define (symbol-expressions-expression ($symbol : Symbol) ($expressions : Expressions)) : Expression
  (expression
    (expressions-syntax
      (if (= (expressions-size $expressions) 1)
        $expressions
        (expressions-do $expressions 
          (lambda (($scope : Scope))
            (expressions 
              (tuple-syntax (map binding-expression $scope))
              (expressions-structure $expressions))))))
    (field $symbol (expressions-structure $expressions))))

(check-equal?
  (expression-sexp-type
    (symbol-expressions-expression `point
      (expressions
        syntax-a
        (structure
          dynamic-type-a 
          static-type-b 
          dynamic-type-c))))
  (pair
    `(let-values (((tmp-a tmp-c) a)) (cons tmp-a tmp-c))
    (field `point 
      (stack 
        dynamic-type-a 
        static-type-b 
        dynamic-type-c))))

; ---------------------------------------------------------------------

(data let-values-entry 
  (temporary-stack : (Stackof Identifier))
  (syntax-option : (Option Syntax))
  (tuple : Tuple))

(define (expressions-let-values-entry
  ($expressions : Expressions))
  : Let-Values-Entry
  (define $syntax (expressions-syntax $expressions))
  (define $structure (expressions-structure $expressions))
  (define $size (structure-dynamic-size $structure))
  (case (structure-dynamic-size $structure)
    ((0)
      (let-values-entry
        null
        #f
        (map
          (lambda (($type : Type)) (expression null-syntax $type))
          $structure)))
    ((1)
      (let-values-entry
        null
        #f
        (map
          (lambda (($type : Type)) (make-expression $syntax $type))
          $structure)))
    (else
      (define $scope (structure-generate-scope $structure))
      (let-values-entry
        (filter-false (map binding-identifier-option $scope))
        $syntax
        (map binding-expression $scope)))))

(parameterize ((testing? #t))
  (bind $let-values-entry
    (expressions-let-values-entry
      (make-expressions #`expr (structure static-type-a)))
    (check-equal?
      (map syntax->datum (let-values-entry-temporary-stack $let-values-entry))
      (stack))
    (check-equal?
      (let-values-entry-syntax-option $let-values-entry)
      #f)
    (check-equal?
      (map expression-sexp-type (let-values-entry-tuple $let-values-entry))
      (stack (pair #f static-type-a))))

  (bind $let-values-entry
    (expressions-let-values-entry
      (make-expressions #`expr (structure dynamic-type-a static-type-b)))
    (check-equal?
      (map syntax->datum (let-values-entry-temporary-stack $let-values-entry))
      (stack))
    (check-equal?
      (let-values-entry-syntax-option $let-values-entry)
      #f)
    (check-equal?
      (map expression-sexp-type (let-values-entry-tuple $let-values-entry))
      (stack 
        (pair `expr dynamic-type-a)
        (pair #f static-type-b))))

  (bind $let-values-entry
    (expressions-let-values-entry
      (make-expressions #`expr (structure dynamic-type-a static-type-b dynamic-type-c)))
    (check-equal?
      (map syntax->datum (let-values-entry-temporary-stack $let-values-entry))
      (stack `tmp-a `tmp-c))
    (check-equal?
      (option-app syntax->datum (let-values-entry-syntax-option $let-values-entry))
      `expr)
    (check-equal?
      (map expression-sexp-type (let-values-entry-tuple $let-values-entry))
      (stack 
        (pair `tmp-a dynamic-type-a)
        (pair #f static-type-b)
        (pair `tmp-c dynamic-type-c)))))

; ---------------------------------------------------------------------

(define (expressions-sexp-option ($expressions : Expressions)) : (Option Sexp)
  (option-app syntax->datum (expressions-syntax-option $expressions)))

(define (scope-doing-expressions
  ($scope : Scope)
  ($expressions : Expressions)) : Expressions
  (expressions
    (make-syntax 
      `(lambda 
        ,(reverse (filter-false (map binding-identifier-option $scope)))
        ,(expressions-syntax $expressions)))
    (structure 
      (arrow
        (scope-structure $scope)
        (expressions-structure $expressions)))))

(check-equal?
  (expressions-sexp-structure
    (scope-doing-expressions
      (scope 
        (binding number-type #`num)
        (binding text-type #`txt))
      (expressions 
        #`(string-append (number->string num) txt)
        (structure text-type))))
  (pair
    `(lambda (num txt) (string-append (number->string num) txt))
    (structure (arrow (structure number-type text-type) (structure text-type)))))
