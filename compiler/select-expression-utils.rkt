#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define (index-syntax-structure-select-expression 
  ($index : Exact-Nonnegative-Integer)
  ($syntax : Syntax)
  ($structure : Structure)) : Expression
  (define $size (length $structure))
  (expression
    (make-syntax
      (case $size
        ((0) `(error))
        ((1) $syntax)
        (else
          (define $selector 
            (if (= $size 2)
              (= $index 0)
              $index))
          (if (structure-dynamic? $structure) 
            `(cons ,$selector ,$syntax) 
            $selector))))
    (choice $structure)))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 0 #`stx null-structure))
  (pair `(error) (choice null-structure)))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 0 null-syntax
      (structure static-type-a)))
  (expression-sexp
    (expression null-syntax (choice! static-type-a))))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 0 #`stx
      (structure dynamic-type-a)))
  (pair `stx (choice (structure dynamic-type-a))))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 0 null-syntax
      (structure static-type-a static-type-b)))
  (pair #t (choice (structure static-type-a static-type-b))))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 1 null-syntax
      (structure static-type-a static-type-b)))
  (pair #f (choice (structure static-type-a static-type-b))))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 0 #`stx
      (structure dynamic-type-a static-type-b)))
  (pair `(cons #t stx) (choice (structure dynamic-type-a static-type-b))))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 1 #`stx
      (structure dynamic-type-a static-type-b)))
  (pair `(cons #f stx) (choice (structure dynamic-type-a static-type-b))))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 
      0
      null-syntax
      (structure static-type-a static-type-b static-type-c)))
  (pair 
    0
    (choice (structure static-type-a static-type-b static-type-c))))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 
      0
      #`stx
      (structure dynamic-type-a static-type-b dynamic-type-c)))
  (pair 
    `(cons 0 stx) 
    (choice (structure dynamic-type-a static-type-b dynamic-type-c))))
