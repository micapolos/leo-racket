#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  racket/list
  leo/typed/option
  leo/typed/testing
  leo/typed/stack
  leo/compiler/racket
  leo/compiler/package
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/tuple-syntax
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-check
  leo/compiler/typed
  leo/compiler/type-utils)

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
    expression-typed-datum
    (package-tuple
      (package
        syntax-a
        (structure dynamic-type-a dynamic-type-b static-type-c dynamic-type-d))))
  (stack
    (typed `(unsafe-vector-ref a 0) dynamic-type-a)
    (typed `(unsafe-vector-ref a 1) dynamic-type-b)
    (typed `#f static-type-c)
    (typed `(unsafe-vector-ref a 2) dynamic-type-d)))

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
  (option-app package-typed-sexp
    (expression-apply-package
      (expression #`fn (arrow (structure type-a) (structure type-b)))
      (package #`pkg (structure type-a))))
  (typed 
    `(call-with-values (lambda () pkg) fn)
    (structure type-b)))

(check-equal?
  (expression-apply-package
    (expression #`fn (arrow (structure type-a) (structure type-b)))
    (package #`pkg (structure type-b)))
  #f)
