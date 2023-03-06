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
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expression-resolve)

(define (expressions-list-tuple-resolve-fn
  ($expressions-list : (Listof Expressions))
  ($tuple : Tuple)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (cond
    ((null? $expressions-list) ($fn $tuple))
    (else 
      (expressions-resolve-fn (car $expressions-list)
        (lambda (($expressions-tuple : Tuple))
          (expressions-list-tuple-resolve-fn
            (cdr $expressions-list)
            (push-stack $tuple $expressions-tuple)
            $fn))))))

(define (package-resolve-fn
  ($package : Package)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (expressions-list-tuple-resolve-fn (reverse $package) null-scope $fn))

(check-equal?
  (option-app expressions-sexp
    (package-resolve-fn
      (package expressions-ab expressions-cd)
      (lambda (($tuple : Tuple)) #f)))
  #f)

(check-equal?
  (option-app expressions-sexp
    (package-resolve-fn
      (package expressions-a)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions result (structure (racket a))))

(check-equal?
  (option-app expressions-sexp
    (package-resolve-fn
      (package expressions-ab expressions-cd)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions 
    (let-values (((tmp-a tmp-b) ab))
      (let-values (((tmp-c tmp-d) cd))
        result))
    (structure (racket a) (racket b) (racket c) (racket d))))

; --------------------------------------------------------------------------------

; (define (symbol-package-expression ($symbol : Symbol) ($package : Package)) : Expression
;   (package-do $package
;     (lambda (($tuple : Tuple))
;       (expression
;         (tuple-syntax $tuple)
;         (field $symbol (tuple-structure $tuple))))))

