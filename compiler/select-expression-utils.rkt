#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define (index-syntax-structure-select-expression 
  ($index : Exact-Nonnegative-Integer)
  ($syntax-option : (Option Syntax))
  ($structure : Structure)) : Expression
  (define $size (length $structure))
  (expression
    (and $syntax-option
      (make-syntax
        (case $size
          ((0) `(error))
          ((1) $syntax-option)
          (else
            (define $selector
              (if (= $size 2)
                (= $index 0)
                $index))
            (if (structure-dynamic? $structure)
              `(cons ,$selector ,$syntax-option)
              $selector)))))
    (choice $structure)))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 0 #`stx null-structure))
  (pair `(error) (choice null-structure)))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 0 #f
      (structure static-type-a)))
  (expression-sexp
    (expression #f (choice! static-type-a))))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 0 #`stx
      (structure dynamic-type-a)))
  (pair `stx (choice (structure dynamic-type-a))))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 0 #f
      (structure static-type-a static-type-b)))
  (pair #f (choice (structure static-type-a static-type-b))))

(check-equal?
  (expression-sexp-type
    (index-syntax-structure-select-expression 1 #f
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
      #f
      (structure static-type-a static-type-b static-type-c)))
  (pair 
    #f
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
