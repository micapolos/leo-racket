#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/binding
  leo/compiler/scope
  leo/compiler/scope-utils
  leo/compiler/expressions-part
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/syntax-utils
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/expression
  leo/compiler/sexp-expression
  leo/compiler/expression-utils
  leo/compiler/expression-resolve)

(define (expressions-part-plus ($lhs-expressions-part : Expressions-Part) ($rhs-expressions-part : Expressions-Part)) : Expressions-Part
  (push-stack $lhs-expressions-part $rhs-expressions-part))

(define (expressions-part-plus-tuple ($expressions-part : Expressions-Part) ($tuple : Tuple)) : Expressions-Part
  (push-stack $expressions-part (map expression-expressions $tuple)))

(define (expressions-part-structure ($expressions-part : Expressions-Part)) : Structure
  (apply append (map expressions-structure $expressions-part)))

(define (expressions-part-resolve-fn
  ($expressions-part : Expressions-Part)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (define $expressions-stack $expressions-part)
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
    (expressions-part-resolve-fn
      (expressions-part expressions-ab expressions-cd)
      (lambda (($tuple : Tuple)) #f)))
  #f)

; resolved to static
(check-equal?
  (option-app expressions-sexp
    (expressions-part-resolve-fn
      (expressions-part expressions-ab expressions-cd)
      (lambda (($tuple : Tuple)) 
        (expressions null-syntax static-structure-a))))
  `(expressions #f (structure a)))

; single-expression
(check-equal?
  (option-app expressions-sexp
    (expressions-part-resolve-fn
      (expressions-part expressions-a)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions result (structure (a racket))))

; single-expression & multi-expression
(check-equal?
  (option-app expressions-sexp
    (expressions-part-resolve-fn
      (expressions-part expressions-a expressions-cd)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions 
    (let-values (((tmp-c tmp-d) cd)) result)
    (structure (a racket) (c racket) (d racket))))

; multi-expressions
(check-equal?
  (option-app expressions-sexp
    (expressions-part-resolve-fn
      (expressions-part expressions-ab expressions-cd)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions 
    (let-values (((tmp-a tmp-b) ab)
                 ((tmp-c tmp-d) cd))
        result)
    (structure (a racket) (b racket) (c racket) (d racket))))

; --------------------------------------------------------------------------------

(define (expressions-part-apply-fn
  ($expressions-part : Expressions-Part)
  ($fn : (-> Tuple Expressions))) : Expressions
  (option-ref (expressions-part-resolve-fn $expressions-part $fn)))

; --------------------------------------------------------------------------------

(define (expressions-part-do ($expressions-part : Expressions-Part) ($fn : (-> Scope Expressions))) : Expressions
  (define $expressions-stack $expressions-part)
  (define $syntax-option-stack (map expressions-syntax-option $expressions-stack))
  (define $structure-stack (map expressions-structure $expressions-stack))
  (define $scope-stack (map structure-generate-scope $structure-stack))
  (define $scope (apply append $scope-stack))
  (define $fn-expressions ($fn $scope))
  (define $identifier-stack-stack (map scope-identifier-stack $scope-stack))
  (make-expressions
    (make-syntax 
      `(let-values
        ,(reverse 
          (filter-false 
            (map
              (lambda (
                ($identifier-stack : (Stackof Identifier)) 
                ($syntax-option : (Option Syntax)))
                (and $syntax-option
                  `(
                    ,(reverse $identifier-stack)
                    ,$syntax-option)))
              $identifier-stack-stack
              $syntax-option-stack)))
        ,(expressions-syntax $fn-expressions)))
    (expressions-structure $fn-expressions)))

; do static
(check-equal?
  (expressions-sexp
    (expressions-part-do
      (expressions-part expressions-ab expressions-cd)
      (lambda (($scope : Scope)) 
        (expressions null-syntax static-structure-a))))
  `(expressions #f (structure a)))

; static-expressions
(check-equal?
  (expressions-sexp
    (expressions-part-do
      (expressions-part static-expressions-a expressions-b)
      (lambda (($scope : Scope))
        (make-expressions #`result (map binding-type $scope)))))
  `(expressions 
    (let-values (((tmp-b) b)) result)
    (structure a (b racket))))

; single-expressions
(check-equal?
  (expressions-sexp
    (expressions-part-do
      (expressions-part expressions-a expressions-b)
      (lambda (($scope : Scope))
        (make-expressions #`result (map binding-type $scope)))))
  `(expressions 
    (let-values (((tmp-a) a) ((tmp-b) b)) result)
    (structure (a racket) (b racket))))

; mutli-expressions
(check-equal?
  (expressions-sexp
    (expressions-part-do
      (expressions-part expressions-ab expressions-cd)
      (lambda (($scope : Scope))
        (make-expressions #`result (map binding-type $scope)))))
  `(expressions 
    (let-values (((tmp-a tmp-b) ab)
                 ((tmp-c tmp-d) cd))
        result)
    (structure (a racket) (b racket) (c racket) (d racket))))

; ----------------------------------------------------------------------------

(define (symbol-expressions-part-expressions ($symbol : Symbol) ($expressions-part : Expressions-Part)) : Expressions
  (expressions-part-apply-fn $expressions-part
    (lambda (($tuple : Tuple))
      (make-expressions
        (tuple-syntax $tuple)
        (structure (field $symbol (tuple-structure $tuple)))))))

(check-equal?
  (expressions-sexp
    (symbol-expressions-part-expressions
      `foo
      (expressions-part expressions-a expressions-cd)))
  `(expressions 
    (let-values (((tmp-c tmp-d) cd))
      (vector a tmp-c tmp-d))
    (structure (foo (a racket) (c racket) (d racket)))))

; --------------------------------------------------------------------------

(define (expressions-part-expressions ($expressions-part : Expressions-Part)) : Expressions
  (expressions-part-apply-fn $expressions-part
    (lambda (($tuple : Tuple))
      (expressions
        (make-syntax
          (bind $syntax-list (reverse (tuple-syntax-stack $tuple))
            (cond
              ((= (length $syntax-list) 1) (car $syntax-list))
              (else `(values ,@$syntax-list)))))
        (tuple-structure $tuple)))))

; single-expression
(check-equal?
  (expressions-sexp
    (expressions-part-expressions
      (expressions-part expressions-a)))
  `(expressions a (structure (a racket))))

; single-expression & multi-expression
(check-equal?
  (expressions-sexp
    (expressions-part-expressions
      (expressions-part expressions-a expressions-cd)))
  `(expressions 
    (let-values (((tmp-c tmp-d) cd)) (values a tmp-c tmp-d))
    (structure (a racket) (c racket) (d racket))))

; ----------------------------------------------------------------------

(define (expressions-part-sexp-list ($expressions-part : Expressions-Part)) : (Listof Sexp)
  (reverse (filter-false (map expressions-sexp-option $expressions-part))))

(define (scope-doing-expressions-part ($scope : Scope) ($expressions-part : Expressions-Part)) : Expressions-Part
  (expressions-part (scope-doing-expressions $scope (expressions-part-expressions $expressions-part))))

(define (expressions-part-apply-type ($expressions-part : Expressions-Part)) : Expressions-Part
  (map expression-expressions
    (map type-expression
      (expressions-part-structure $expressions-part))))

(define (expressions-part-apply-racket ($expressions-part : Expressions-Part)) : Expressions-Part
  (expressions-part
    (expressions 
      (make-syntax 
        `(quote 
          ,(reverse 
            (filter-false 
              (map expressions-syntax-option $expressions-part)))))
      (structure (racket)))))

(define (expressions-part-lift-structure ($expressions-part : Expressions-Part)) : (Option Structure)
  (structure-lift (expressions-part-structure $expressions-part)))
