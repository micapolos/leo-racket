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
  leo/compiler/ingredients
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

(define (ingredients-plus ($lhs-ingredients : Ingredients) ($rhs-ingredients : Ingredients)) : Ingredients
  (push-stack $lhs-ingredients $rhs-ingredients))

(define (ingredients-plus-tuple ($ingredients : Ingredients) ($tuple : Tuple)) : Ingredients
  (push-stack $ingredients (map expression-expressions $tuple)))

(define (ingredients-structure ($ingredients : Ingredients)) : Structure
  (apply append (map expressions-structure $ingredients)))

(define (ingredients-resolve-fn
  ($ingredients : Ingredients)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (define $expressions-stack $ingredients)
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
    (ingredients-resolve-fn
      (ingredients expressions-ab expressions-cd)
      (lambda (($tuple : Tuple)) #f)))
  #f)

; resolved to static
(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients expressions-ab expressions-cd)
      (lambda (($tuple : Tuple)) 
        (expressions null-syntax static-structure-a))))
  `(expressions #f (structure a)))

; single-expression
(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients expressions-a)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions result (structure (a racket))))

; single-expression & multi-expression
(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients expressions-a expressions-cd)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions 
    (let-values (((tmp-c tmp-d) cd)) result)
    (structure (a racket) (c racket) (d racket))))

; multi-expressions
(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients expressions-ab expressions-cd)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions 
    (let-values (((tmp-a tmp-b) ab)
                 ((tmp-c tmp-d) cd))
        result)
    (structure (a racket) (b racket) (c racket) (d racket))))

; --------------------------------------------------------------------------------

(define (ingredients-apply-fn
  ($ingredients : Ingredients)
  ($fn : (-> Tuple Expressions))) : Expressions
  (option-ref (ingredients-resolve-fn $ingredients $fn)))

; --------------------------------------------------------------------------------

(define (ingredients-do ($ingredients : Ingredients) ($fn : (-> Scope Expressions))) : Expressions
  (define $expressions-stack $ingredients)
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
    (ingredients-do
      (ingredients expressions-ab expressions-cd)
      (lambda (($scope : Scope)) 
        (expressions null-syntax static-structure-a))))
  `(expressions #f (structure a)))

; static-expressions
(check-equal?
  (expressions-sexp
    (ingredients-do
      (ingredients static-expressions-a expressions-b)
      (lambda (($scope : Scope))
        (make-expressions #`result (map binding-type $scope)))))
  `(expressions 
    (let-values (((tmp-b) b)) result)
    (structure a (b racket))))

; single-expressions
(check-equal?
  (expressions-sexp
    (ingredients-do
      (ingredients expressions-a expressions-b)
      (lambda (($scope : Scope))
        (make-expressions #`result (map binding-type $scope)))))
  `(expressions 
    (let-values (((tmp-a) a) ((tmp-b) b)) result)
    (structure (a racket) (b racket))))

; mutli-expressions
(check-equal?
  (expressions-sexp
    (ingredients-do
      (ingredients expressions-ab expressions-cd)
      (lambda (($scope : Scope))
        (make-expressions #`result (map binding-type $scope)))))
  `(expressions 
    (let-values (((tmp-a tmp-b) ab)
                 ((tmp-c tmp-d) cd))
        result)
    (structure (a racket) (b racket) (c racket) (d racket))))

; ----------------------------------------------------------------------------

(define (symbol-ingredients-expressions ($symbol : Symbol) ($ingredients : Ingredients)) : Expressions
  (ingredients-apply-fn $ingredients
    (lambda (($tuple : Tuple))
      (make-expressions
        (tuple-syntax $tuple)
        (structure (field $symbol (tuple-structure $tuple)))))))

(check-equal?
  (expressions-sexp
    (symbol-ingredients-expressions
      `foo
      (ingredients expressions-a expressions-cd)))
  `(expressions 
    (let-values (((tmp-c tmp-d) cd))
      (vector a tmp-c tmp-d))
    (structure (foo (a racket) (c racket) (d racket)))))

; --------------------------------------------------------------------------

(define (ingredients-expressions ($ingredients : Ingredients)) : Expressions
  (ingredients-apply-fn $ingredients
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
    (ingredients-expressions
      (ingredients expressions-a)))
  `(expressions a (structure (a racket))))

; single-expression & multi-expression
(check-equal?
  (expressions-sexp
    (ingredients-expressions
      (ingredients expressions-a expressions-cd)))
  `(expressions 
    (let-values (((tmp-c tmp-d) cd)) (values a tmp-c tmp-d))
    (structure (a racket) (c racket) (d racket))))

; ----------------------------------------------------------------------

(define (ingredients-sexp-list ($ingredients : Ingredients)) : (Listof Sexp)
  (reverse (filter-false (map expressions-sexp-option $ingredients))))

(define (scope-doing-ingredients ($scope : Scope) ($ingredients : Ingredients)) : Ingredients
  (ingredients (scope-doing-expressions $scope (ingredients-expressions $ingredients))))

(define (ingredients-apply-type ($ingredients : Ingredients)) : Ingredients
  (map expression-expressions
    (map type-expression
      (ingredients-structure $ingredients))))

(define (ingredients-apply-racket ($ingredients : Ingredients)) : Ingredients
  (ingredients
    (expressions 
      (make-syntax 
        `(quote 
          ,(reverse 
            (filter-false 
              (map expressions-syntax-option $ingredients)))))
      (structure (racket)))))

(define (ingredients-lift-structure ($ingredients : Ingredients)) : (Option Structure)
  (structure-lift (ingredients-structure $ingredients)))