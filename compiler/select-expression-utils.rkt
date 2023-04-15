#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define (index-syntax-structure-select-expression-option
  ($index : Exact-Nonnegative-Integer)
  ($syntax-option : (Option Syntax))
  ($structure : Structure))
: (Option Expression)
  (define $size (length $structure))
  (and (not (= $size 0))
    (expression
      (make-syntax
        (case $size
          ((1) $syntax-option)
          (else
            (define $selector
              (if (= $size 2)
                (= $index 0)
                $index))
            (if (structure-dynamic? $structure)
              `(cons ,$selector ,(or $syntax-option #f))
              $selector))))
      (choice $structure))))

(check-equal?
  (option-app expression-sexp
    (index-syntax-structure-select-expression-option 0 #`stx null-structure))
  #f)

(check-equal?
  (option-app expression-sexp
    (index-syntax-structure-select-expression-option 0 #f
      (structure static-type-a)))
  (expression-sexp
    (expression #f (choice! static-type-a))))

(check-equal?
  (option-app expression-sexp
    (index-syntax-structure-select-expression-option 0 #`stx
      (structure dynamic-type-a)))
  (expression-sexp
    (expression #`stx (choice! dynamic-type-a))))

(check-equal?
  (option-app expression-sexp
    (index-syntax-structure-select-expression-option 0 #f
      (structure static-type-a static-type-b)))
  (expression-sexp
    (expression #`#t (choice! static-type-a static-type-b))))

(check-equal?
  (option-app expression-sexp
    (index-syntax-structure-select-expression-option 1 #f
      (structure static-type-a static-type-b)))
  (expression-sexp
    (expression #`#f (choice! static-type-a static-type-b))))

(check-equal?
  (option-app expression-sexp
    (index-syntax-structure-select-expression-option 0 #`stx
      (structure dynamic-type-a static-type-b)))
  (expression-sexp
    (expression
      #`(cons #t stx)
      (choice! dynamic-type-a static-type-b))))

(check-equal?
  (option-app expression-sexp
    (index-syntax-structure-select-expression-option 1 #`stx
      (structure dynamic-type-a static-type-b)))
  (expression-sexp
    (expression
      #`(cons #f stx)
      (choice! dynamic-type-a static-type-b))))

(check-equal?
  (option-app expression-sexp
    (index-syntax-structure-select-expression-option
      0
      #f
      (structure static-type-a static-type-b static-type-c)))
  (expression-sexp
    (expression
      #`0
      (choice! static-type-a static-type-b static-type-c))))

(check-equal?
  (option-app expression-sexp
    (index-syntax-structure-select-expression-option
      0
      #`stx
      (structure dynamic-type-a static-type-b dynamic-type-c)))
  (expression-sexp
    (expression
      #`(cons 0 stx)
      (choice! dynamic-type-a static-type-b dynamic-type-c))))
