#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  racket/list
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/typed/stack
  leo/compiler/racket
  leo/compiler/binding-utils
  leo/compiler/sexp-utils
  leo/compiler/binding
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/expressions
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-check
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

; ----------------------------------------------------------------------------

(define (expressions-ref
  ($expressions : Expressions)
  ($index : Exact-Nonnegative-Integer))
  : Expression
  (define $syntax (expressions-syntax $expressions))
  (define $structure (expressions-structure $expressions))
  (define $structure-compiled-size (structure-compiled-size $structure))
  (define $dynamic-index (structure-dynamic-ref $structure $index))
  (define $type (list-ref $structure $index))
  (expression
    (make-syntax
      (and
        $dynamic-index
        (case $structure-compiled-size
          ((0) #f)
          ((1) $syntax)
          ((2)
            `(
              ,(if (= $dynamic-index 1) `unsafe-car `unsafe-cdr)
              ,$syntax))
          (else
            `(unsafe-vector-ref 
              ,$syntax
              ,(- $structure-compiled-size $dynamic-index 1))))))
    $type))

(define (expressions-tuple ($expressions : Expressions)) : Tuple
  (map 
    (curry expressions-ref $expressions)
    (range (expressions-size $expressions))))

(check-equal?
  (map
    expression-sexp-type
    (expressions-tuple
      (expressions
        syntax-a
        (structure dynamic-type-a dynamic-type-b static-type-c dynamic-type-d))))
  (stack
    (pair `(unsafe-vector-ref a 0) dynamic-type-a)
    (pair `(unsafe-vector-ref a 1) dynamic-type-b)
    (pair `#f static-type-c)
    (pair `(unsafe-vector-ref a 2) dynamic-type-d)))

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

(define (expressions-rhs-option ($expressions : Expressions)) : (Option Expressions)
  (option-bind (single (expressions-structure $expressions)) $type
    (and (field? $type)
      (expressions 
        (expressions-syntax $expressions) 
        (field-structure $type)))))

(check-equal?
  (expressions-rhs-option
    (expressions syntax-a 
      (structure 
        (field `foo
          (structure type-b type-c)))))
  (expressions syntax-a (structure type-b type-c)))

(check-equal?
  (expressions-rhs-option
    (expressions syntax-a 
      (structure 
        (field `foo null)
        (field `bar null))))
  #f)

(check-equal?
  (expressions-rhs-option
    (expressions syntax-a (structure (racket `foo))))
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
  (define $compiled-size (structure-compiled-size $structure))
  (case $compiled-size
    ((0 1) 
      ($fn (map (curry expression $syntax) $structure)))
    (else 
      (define $scope (structure-generate-scope $structure))
      (define $tmp-stack (scope-identifier-stack $scope))
      (define $fn-expressions ($fn (map binding-expression $scope)))
      (and $fn-expressions
        (make-expressions
          (make-syntax 
            `(let-values
              (((,@(reverse $tmp-stack)) ,$syntax))
              ,(expressions-syntax $fn-expressions)))
          (expressions-structure $fn-expressions))))))

(check-equal?
  (option-app expressions-sexp-structure
    (expressions-resolve-fn expressions-a
      (lambda (($tuple : Tuple))
        (expressions
          (make-syntax (reverse (map expression-syntax $tuple)))
          (tuple-structure $tuple)))))
  (pair 
    `(a)
    structure-a))

(check-equal?
  (option-app expressions-sexp-structure
    (expressions-resolve-fn expressions-ab
      (lambda (($tuple : Tuple))
        (expressions 
          (make-syntax (reverse (map expression-syntax $tuple)))
          (tuple-structure $tuple)))))
  (pair 
    `(let-values (((tmp-a tmp-b) ab)) (tmp-a tmp-b))
    structure-ab))

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
  (define $size (structure-compiled-size $structure))
  (case (structure-compiled-size $structure)
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