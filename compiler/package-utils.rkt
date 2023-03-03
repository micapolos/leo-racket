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
  leo/compiler/expression-stack-syntax
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/typed
  leo/compiler/type-utils)

(define (package-size ($package : Package)) : Exact-Nonnegative-Integer
  (length (package-structure $package)))

; ----------------------------------------------------------------------------

(define (package-unsafe-ref
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

(define (package-expression-stack ($package : Package)) : (Stackof Expression)
  (map 
    (curry package-unsafe-ref $package)
    (range (package-size $package))))

(check-equal?
  (map
    expression-typed-datum
    (package-expression-stack
      (package
        syntax-a
        (structure dynamic-type-a dynamic-type-b static-type-c dynamic-type-d))))
  (stack
    (typed `(unsafe-vector-ref a 0) dynamic-type-a)
    (typed `(unsafe-vector-ref a 1) dynamic-type-b)
    (typed `#f static-type-c)
    (typed `(unsafe-vector-ref a 2) dynamic-type-d)))

(define (expression-package ($expression : Expression)) : Package
  (expression-stack-package (stack $expression)))

(define (expression-stack-package ($expression-stack : (Stackof Expression))) : Package
  (package
    (expression-stack-syntax $expression-stack)
    (map expression-type $expression-stack)))

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

; ------------------------------------------------------------------

(define (package-typed-sexp ($package : Package)) : (Typed Sexp Structure)
  (typed
    (syntax->datum (package-syntax $package))
    (package-structure $package)))
