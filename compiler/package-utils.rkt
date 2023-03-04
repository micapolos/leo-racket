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
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/package
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/tuple-syntax
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-check
  leo/compiler/typed
  leo/compiler/type-utils)

(define null-package (package null-syntax null-structure))

(define (package-size ($package : Package)) : Exact-Nonnegative-Integer
  (length (package-structure $package)))

; ----------------------------------------------------------------------------

(define (package-ref
  ($package : Package)
  ($index : Exact-Nonnegative-Integer))
  : Expression
  (define $syntax (package-syntax $package))
  (define $structure (package-structure $package))
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

(define (package-tuple ($package : Package)) : Tuple
  (map 
    (curry package-ref $package)
    (range (package-size $package))))

(check-equal?
  (map
    expression-sexp-type
    (package-tuple
      (package
        syntax-a
        (structure dynamic-type-a dynamic-type-b static-type-c dynamic-type-d))))
  (stack
    (pair `(unsafe-vector-ref a 0) dynamic-type-a)
    (pair `(unsafe-vector-ref a 1) dynamic-type-b)
    (pair `#f static-type-c)
    (pair `(unsafe-vector-ref a 2) dynamic-type-d)))

(define (expression-package ($expression : Expression)) : Package
  (tuple-package (stack $expression)))

(define (tuple-package ($tuple : Tuple)) : Package
  (package
    (tuple-syntax $tuple)
    (map expression-type $tuple)))

; -------------------------------------------------------------------

(define (package-rhs-option ($package : Package)) : (Option Package)
  (option-bind (single (package-structure $package)) $type
    (and (field? $type)
      (package 
        (package-syntax $package) 
        (field-structure $type)))))

(check-equal?
  (package-rhs-option
    (package syntax-a 
      (structure 
        (field `foo
          (structure type-b type-c)))))
  (package syntax-a (structure type-b type-c)))

(check-equal?
  (package-rhs-option
    (package syntax-a 
      (structure 
        (field `foo null)
        (field `bar null))))
  #f)

(check-equal?
  (package-rhs-option
    (package syntax-a (structure (racket `foo))))
  #f)

; --------------------------------------------------------------

(define (expression-apply-package
  ($lhs-expression : Expression)
  ($rhs-package : Package))
  : (Option Package)
  (option-app package
    (make-syntax
      `(call-with-values
        (lambda () ,(package-syntax $rhs-package))
        ,(expression-syntax $lhs-expression)))
    (type-apply-structure 
      (expression-type $lhs-expression)
      (package-structure $rhs-package))))

(check-equal?
  (option-app package-sexp-structure
    (expression-apply-package
      (expression #`fn (arrow (structure type-a) (structure type-b)))
      (package #`pkg (structure type-a))))
  (pair 
    `(call-with-values (lambda () pkg) fn)
    (structure type-b)))

(check-equal?
  (expression-apply-package
    (expression #`fn (arrow (structure type-a) (structure type-b)))
    (package #`pkg (structure type-b)))
  #f)

; ---------------------------------------------------------------

(define (package-do ($package : Package) ($fn : (-> Scope Package))) : Package
  (define $syntax (package-syntax $package))
  (define $structure (package-structure $package))
  (define $scope (structure-generate-scope $structure))
  (define $fn-package ($fn $scope))
  (define $fn-syntax (package-syntax $fn-package))
  (define $fn-structure (package-structure $fn-package))
  (define $tmp-stack (scope-identifier-stack $scope))
  (package
    (make-syntax 
      (case (length $tmp-stack)
        ((0) $fn-syntax)
        ((1) 
          `(let
            ((,(car $tmp-stack) ,$syntax))
            ,$fn-syntax))
        (else 
          `(let-values 
            ((,@(reverse $tmp-stack)) ,$syntax) 
            ,$fn-syntax))))
    $fn-structure))

(parameterize ((tmp-temporaries? #t))
  (check-equal?
    (package-sexp-structure
      (package-do
        (package #`pkg (structure static-type-a))
        (lambda (($scope : Scope)) 
          (package 
            (make-syntax `(values ,@(scope-identifier-stack $scope)))
            (reverse (scope-structure $scope))))))
    (pair 
      `(values)
      (structure static-type-a)))

  (check-equal?
    (package-sexp-structure
      (package-do
        (package #`pkg (structure dynamic-type-a static-type-b))
        (lambda (($scope : Scope)) 
          (package 
            (make-syntax `(values ,@(scope-identifier-stack $scope)))
            (reverse (scope-structure $scope))))))
    (pair 
      `(let ((tmp-a pkg)) (values tmp-a)) 
      (structure static-type-b dynamic-type-a)))

  (check-equal?
    (package-sexp-structure
      (package-do
        (package #`pkg 
          (structure 
            dynamic-type-a
            static-type-b
            dynamic-type-c))
        (lambda (($scope : Scope)) 
          (package 
            (make-syntax `(values ,@(scope-identifier-stack $scope)))
            (reverse (scope-structure $scope))))))
    (pair 
      `(let-values ((tmp-a tmp-c) pkg) (values tmp-c tmp-a)) 
      (structure dynamic-type-c static-type-b dynamic-type-a))))
