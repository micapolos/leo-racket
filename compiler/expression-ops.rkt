#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define-type Expression-Index-Get-Error (U 'not-field 'invalid-index))

(define (expression-index-get
  ($expression : Expression)
  ($index : Exact-Nonnegative-Integer))
: (Result Expression Expression-Index-Get-Error)
  (define $type (expression-type $expression))
  (cond
    ((field? $type)
      (define $syntax-option (expression-syntax-option $expression))
      (define $structure (field-structure $type))
      (cond
        ((< $index (length $structure))
          (result (success (syntax-option-structure-ref $syntax-option $structure $index))))
        (else
          (result (failure `invalid-index)))))
    (else
      (result (failure `not-field)))))

(check-equal?
  (result-app expression-sexp
    (expression-index-get
      (expression syntax-a (field! `foo text-type number-type))
      0))
  (result
    (success
      (expression-sexp
        (syntax-option-structure-ref
          syntax-a
          (structure text-type number-type)
          0)))))

(check-equal?
  (result-app expression-sexp
    (expression-index-get
      (expression syntax-a (field! `foo text-type number-type))
      2))
  (result (failure `invalid-index)))

(check-equal?
  (result-app expression-sexp
    (expression-index-get
      (expression syntax-a (racket))
      0))
  (result (failure `not-field)))
