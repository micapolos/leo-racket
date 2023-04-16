#lang leo/typed

(require
  leo/compiler/expression
  leo/compiler/type
  leo/compiler/type-match
  leo/compiler/syntax-type-content-tuple)

(data context
  (scope : Scope))

(define (context-expression-rhs-tuple-option ($context : Context) ($expression : Expression)) : (Option Tuple)
  (define $type (expression-type $expression))
    (cond
      ((type-matches? $type (field! `type (racket)))
        (option-bind (expression-syntax-option $expression) $syntax
          (syntax-type-content-tuple $syntax)))
      (else
        (expression-rhs-tuple-option $expression))))
