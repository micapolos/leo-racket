#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/type
  leo/compiler/syntax-utils)

(define-type Expression-Field (U Field 'expression-type-not-field))

(define (expression-field ($expression : Expression)) : Expression-Field
  (bind $type (expression-type $expression)
    (cond
      ((field? $type) $type)
      (else `expression-type-not-field))))

(check-equal?
  (expression-field
    (expression syntax-a (field! `foo)))
  (field! `foo))

(check-equal?
  (expression-field
    (expression syntax-a (racket)))
  'expression-type-not-field)
