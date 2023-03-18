#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/compiler/generate-temporary
  leo/compiler/syntax-utils
  leo/compiler/type-utils
  leo/compiler/expression
  leo/compiler/expression-utils)

(data binder-entry
  (identifier : Identifier)
  (syntax : Syntax))

(data binder
  (entry-option : (Option Binder-Entry))
  (bound-syntax : Syntax))

(define (binder-entry-sexp ($binder-entry : Binder-Entry)) : Sexp
  `(entry
    (identifier ,(syntax-e (binder-entry-identifier $binder-entry)))
    (syntax ,(syntax->datum (binder-entry-syntax $binder-entry)))))

(define (binder-sexp ($binder : Binder)) : Sexp
  `(binder
    ,(option-app binder-entry-sexp (binder-entry-option $binder))
    (bound (syntax ,(syntax->datum (binder-bound-syntax $binder))))))

(define (expression-binder ($expression : Expression)) : Binder
  (define $type (expression-type $expression))
  (define $syntax (expression-syntax $expression))
  (define $tmp (type-generate-temporary-option $type))
  (or
    (and $tmp (syntax-complex? $syntax) (binder (binder-entry $tmp $syntax) $tmp))
    (binder #f $syntax)))

(check-equal?
  (binder-sexp
    (expression-binder 
      (expression atomic-syntax-a static-type-b)))
  `(binder #f (bound #'atomic-a)))

(check-equal?
  (binder-sexp 
    (expression-binder 
      (expression atomic-syntax-a dynamic-type-b)))
  `(binder #f (bound #'atomic-a)))

(check-equal?
  (binder-sexp 
    (expression-binder 
      (expression complex-syntax-a dynamic-type-b)))
  `(binder 
    (entry (identifier tmp-b) #'(complex-a))
    (bound #'tmp-b)))
