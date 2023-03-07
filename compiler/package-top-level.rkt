#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/package
  leo/compiler/package-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/module-syntax)

(define (package-top-level-syntax ($package : Package)) : Syntax
  (expressions-syntax (package-top-level-expressions $package)))

(define (package-top-level-expressions ($package : Package)) : Expressions
  (package-apply-fn $package tuple-top-level-expressions))

(define (tuple-top-level-expressions ($tuple : Tuple)) : Expressions
  (tuple-expressions (tuple-top-level $tuple)))

(define (tuple-top-level ($tuple : Tuple)) : Tuple
  (map expression-top-level $tuple))

(define (expression-top-level ($expression : Expression)) : Expression
  (expression
    (make-syntax
      `(value-sexp
        (value 
          ,(expression-syntax $expression) 
          ,(type-syntax (expression-type $expression)))))
    (racket)))

(check-equal?
  (syntax->datum
    (package-top-level-syntax
      (package
        (expressions #`expr
          (structure number-type text-type)))))
  `(let-values (((tmp-number tmp-text) expr))
    (values
      (value-sexp (value tmp-number (field 'number (structure (racket)))))
      (value-sexp (value tmp-text (field 'text (structure (racket))))))))
