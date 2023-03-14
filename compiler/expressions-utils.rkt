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
    (or (tuple-expressions-syntax-option $tuple) null-syntax)
    (tuple-structure $tuple)))

(check-equal?
  (expressions-sexp (tuple-expressions (tuple static-expression-a)))
  `(expressions #f (structure a)))

(check-equal?
  (expressions-sexp-structure
    (tuple-expressions
      (tuple dynamic-expression-a static-expression-b)))
  (pair `(dynamic-a) (structure dynamic-type-a static-type-b)))

(check-equal?
  (expressions-sexp
    (tuple-expressions
      (tuple dynamic-expression-a static-expression-b atomic-expression-c)))
  `(expressions
    (values (dynamic-a) atomic-c)
    (structure (a racket) b (c racket))))

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
    (pair `(unsafe-car (dynamic-a)) type-b)
    (pair `(unsafe-cdr (dynamic-a)) type-c)))

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
        ((0) (expressions-syntax $fn-expressions))
        ;((1) (expressions-syntax $fn-expressions))
        (else 
          (make-syntax 
            `(let-values
              ((,(reverse $tmp-stack) ,$syntax))
              ,(expressions-syntax $fn-expressions)))))
      (expressions-structure $fn-expressions))))

(check-equal?
  (option-app expressions-sexp
    (expressions-resolve-fn expressions-a
      (lambda (($tuple : Tuple))
        (expression-expressions 
          (field-expression `resolved $tuple)))))
  `(expressions
    (let-values (((tmp-a) a)) tmp-a)
    (structure (resolved (a racket)))))

(check-equal?
  (option-app expressions-sexp
    (expressions-resolve-fn expressions-ab
      (lambda (($tuple : Tuple))
        (expression-expressions 
          (field-expression `resolved $tuple)))))
  `(expressions
    (let-values (((tmp-a tmp-b) ab)) (cons tmp-a tmp-b))
    (structure (resolved (a racket) (b racket)))))

; ---------------------------------------------------------------------

(define (expressions-apply-fn ($expressions : Expressions) ($fn : (-> Tuple Expressions))) : Expressions
  (option-ref-or
    (expressions-resolve-fn $expressions $fn)
    $expressions))

; ---------------------------------------------------------------------

(define (expressions-do ($expressions : Expressions) ($fn : (-> Tuple Expressions))) : Expressions
  (define $syntax (expressions-syntax $expressions))
  (define $structure (expressions-structure $expressions))
  (define $scope (structure-generate-scope $structure))
  (define $fn-expressions ($fn (scope-tuple $scope)))
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
  (expressions-sexp
    (expressions-do
      (make-expressions #`pkg (structure static-type-a))
      (lambda (($tuple : Tuple)) 
        (expression-expressions
          (field-expression `done $tuple)))))
  `(expressions #f (structure (done a))))

(check-equal?
  (expressions-sexp
    (expressions-do
      (expressions #`pkg (structure dynamic-type-a static-type-b))
      (lambda (($tuple : Tuple)) 
        (expression-expressions
          (field-expression `done $tuple)))))
  `(expressions (let ((tmp-a pkg)) tmp-a) (structure (done (a racket) b))))

(check-equal?
  (expressions-sexp
    (expressions-do
      (expressions #`pkg 
        (structure 
          dynamic-type-a
          static-type-b
          dynamic-type-c))
      (lambda (($tuple : Tuple)) 
        (expression-expressions
          (field-expression `done $tuple)))))
  `(expressions
    (let-values (((tmp-a tmp-c) pkg)) (cons tmp-a tmp-c))
    (structure (done (a racket) b (c racket)))))

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
          (lambda (($tuple : Tuple))
            (expressions 
              (tuple-syntax $tuple)
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
    `(let-values (((tmp-a tmp-c) (dynamic-a))) (cons tmp-a tmp-c))
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
