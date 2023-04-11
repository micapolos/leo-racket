#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expression-field
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define-type Expression-Index-Get
  (U
    Expression
    'expression-type-not-field
    'invalid-index))

(define (expression-get-index
  ($expression : Expression)
  ($index : Exact-Nonnegative-Integer))
: Expression-Index-Get
  (define $field (expression-field $expression))
  (cond
    ((field? $field)
      (define $syntax-option (expression-syntax-option $expression))
      (define $structure (field-structure $field))
      (cond
        ((< $index (length $structure))
          (syntax-option-structure-ref $syntax-option $structure $index))
        (else
          'invalid-index)))
    (else $field)))

(check-equal?
  (app-if expression? expression-sexp
    (expression-get-index
      (expression syntax-a (field! `foo text-type number-type))
      0))
  (expression-sexp
    (syntax-option-structure-ref
      syntax-a
      (structure text-type number-type)
        0)))

(check-equal?
  (app-if expression? expression-sexp
    (expression-get-index
      (expression syntax-a (field! `foo text-type number-type))
      2))
  `invalid-index)

(check-equal?
  (app-if expression? expression-sexp
    (expression-get-index
      (expression syntax-a (racket))
      0))
  `expression-type-not-field)
