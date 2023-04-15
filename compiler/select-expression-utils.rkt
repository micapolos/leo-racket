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
  ($structure : Structure))
: Expression
  (define $size (length $structure))
  (expression
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
            `(cons ,$selector ,(or $syntax-option #f))
            $selector))))
    (choice $structure)))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 0 #`stx null-structure))
  (expression-sexp
    (expression #`(error) (choice!))))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 0 #f
      (structure static-type-a)))
  (expression-sexp
    (expression #f (choice! static-type-a))))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 0 #`stx
      (structure dynamic-type-a)))
  (expression-sexp
    (expression #`stx (choice! dynamic-type-a))))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 0 #f
      (structure static-type-a static-type-b)))
  (expression-sexp
    (expression #`#t (choice! static-type-a static-type-b))))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 1 #f
      (structure static-type-a static-type-b)))
  (expression-sexp
    (expression #`#f (choice! static-type-a static-type-b))))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 0 #`stx
      (structure dynamic-type-a static-type-b)))
  (expression-sexp
    (expression
      #`(cons #t stx)
      (choice! dynamic-type-a static-type-b))))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 1 #`stx
      (structure dynamic-type-a static-type-b)))
  (expression-sexp
    (expression
      #`(cons #f stx)
      (choice! dynamic-type-a static-type-b))))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 
      0
      #f
      (structure static-type-a static-type-b static-type-c)))
  (expression-sexp
    (expression
      #`0
      (choice! static-type-a static-type-b static-type-c))))

(check-equal?
  (expression-sexp
    (index-syntax-structure-select-expression 
      0
      #`stx
      (structure dynamic-type-a static-type-b dynamic-type-c)))
  (expression-sexp
    (expression
      #`(cons 0 stx)
      (choice! dynamic-type-a static-type-b dynamic-type-c))))

; ---------------------------------------------------------

(define (expression-choice-cast ($expression : Expression) ($choice : Choice)) : (Option Expression)
  (define $structure (choice-type-stack $choice))
  (option-bind (structure-index-matching-type $structure (expression-type $expression)) $index
  (index-syntax-structure-select-expression
    $index
    (expression-syntax-option $expression)
    $structure)))

(bind $choice (choice! (field! `foo) (field! `bar) (field! `goo))
  (check-equal?
    (option-app expression-sexp
      (expression-choice-cast (field-expression! foo) $choice))
    (expression-sexp
      (expression #`2 $choice))))
