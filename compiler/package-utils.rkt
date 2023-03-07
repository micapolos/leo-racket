#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/package
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/syntax-utils
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expression-resolve)

(define (package-resolve-fn
  ($package : Package)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (define $expressions-stack $package)
  (define $let-values-entry-stack
    (map expressions-let-values-entry $expressions-stack))
  (define $tuple 
    (apply append (map let-values-entry-tuple $let-values-entry-stack)))
  (option-bind ($fn $tuple) $expressions
    (define $entries 
      (filter-false
        (map 
          (lambda (($let-values-entry : Let-Values-Entry))
            (option-app list
              (reverse (let-values-entry-temporary-stack $let-values-entry))
              (let-values-entry-syntax-option $let-values-entry)))
          (reverse $let-values-entry-stack))))
    (make-expressions
      (make-syntax
        (cond 
          ((null? $entries) (expressions-syntax $expressions))
          (else `(let-values ,$entries ,(expressions-syntax $expressions)))))
      (expressions-structure $expressions))))

; not resolved
(check-equal?
  (option-app expressions-sexp
    (package-resolve-fn
      (package expressions-ab expressions-cd)
      (lambda (($tuple : Tuple)) #f)))
  #f)

; resolved to static
(check-equal?
  (option-app expressions-sexp
    (package-resolve-fn
      (package expressions-ab expressions-cd)
      (lambda (($tuple : Tuple)) 
        (expressions null-syntax static-structure-a))))
  `(expressions #f (structure a)))

; single-expression
(check-equal?
  (option-app expressions-sexp
    (package-resolve-fn
      (package expressions-a)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions result (structure (racket a))))

; single-expression & multi-expression
(check-equal?
  (option-app expressions-sexp
    (package-resolve-fn
      (package expressions-a expressions-cd)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions 
    (let-values (((tmp-c tmp-d) cd)) result)
    (structure (racket a) (racket c) (racket d))))

; multi-expressions
(check-equal?
  (option-app expressions-sexp
    (package-resolve-fn
      (package expressions-ab expressions-cd)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions 
    (let-values (((tmp-a tmp-b) ab)
                 ((tmp-c tmp-d) cd))
        result)
    (structure (racket a) (racket b) (racket c) (racket d))))

; --------------------------------------------------------------------------------

; (define (symbol-package-expression ($symbol : Symbol) ($package : Package)) : Expression
;   (package-do $package
;     (lambda (($tuple : Tuple))
;       (expression
;         (tuple-syntax $tuple)
;         (field $symbol (tuple-structure $tuple))))))

