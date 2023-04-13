#lang leo/typed

(require 
  leo/compiler/ingredients
  leo/compiler/binding
  leo/compiler/type
  leo/compiler/type-match
  leo/compiler/type-sexp
  leo/compiler/type-utils
  leo/compiler/syntax-utils
  leo/compiler/expressions
  leo/compiler/binder
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/expression
  leo/compiler/sexp-expression
  leo/compiler/expression-utils
  leo/compiler/sexp-utils)

(define (ingredients-plus ($lhs-ingredients : Ingredients) ($rhs-ingredients : Ingredients)) : Ingredients
  (push-stack $lhs-ingredients $rhs-ingredients))

(define (ingredients-plus-tuple ($ingredients : Ingredients) ($tuple : Tuple)) : Ingredients
  (push-stack $ingredients (map expression-expressions $tuple)))

(define (ingredients-structure ($ingredients : Ingredients)) : Structure
  (apply append (map expressions-structure $ingredients)))

(define (usage-ingredients-resolve-fn
  ($usage : Usage)
  ($ingredients : Ingredients)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (define $binder-stack (usage-ingredients-binder-stack $usage $ingredients))
  (define $tuple-stack (map binder-tuple $binder-stack))
  (define $tuple (apply append $tuple-stack))
  (option-bind ($fn $tuple) $expressions
    (define $syntax-option (expressions-syntax-option $expressions))
    (define $entry-stack (filter-false (map binder-entry-option $binder-stack)))
    (expressions
      (and $syntax-option (entry-stack-do-syntax $entry-stack $syntax-option))
      (expressions-structure $expressions))))

(define (ingredients-resolve-fn
  ($ingredients : Ingredients)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (usage-ingredients-resolve-fn 'indirect $ingredients $fn))

(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients 
        (expressions #f (structure static-type-a static-type-b))
        (expressions #f (structure static-type-c static-type-d)))
      (lambda (($tuple : Tuple)) 
        (expressions syntax-a (structure dynamic-type-a)))))
  (expressions-sexp
    (expressions syntax-a (structure dynamic-type-a))))

(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients 
        (expressions #f (structure static-type-a static-type-b))
        (expressions #f (structure static-type-c static-type-d)))
      tuple-default-apply-fn))
  (expressions-sexp
    (expressions #f
      (structure
        (field! `resolved 
          static-type-a 
          static-type-b
          static-type-c
          static-type-d)))))

(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients 
        (expressions syntax-a (structure dynamic-type-a dynamic-type-b))
        (expressions syntax-b (structure dynamic-type-c dynamic-type-d)))
      tuple-default-apply-fn))
  (expressions-sexp
    (expressions
      #`(let-values (((tmp-a tmp-b) a) ((tmp-c tmp-d) b))
         (vector tmp-a tmp-b tmp-c tmp-d))
      (structure
        (field! `resolved
          dynamic-type-a
          dynamic-type-b
          dynamic-type-c
          dynamic-type-d)))))

; --------------------------------------------------------------------------------

(define (ingredients-do
  ($ingredients : Ingredients)
  ($fn : (-> Scope Expressions))) : Expressions
  (define $scoper-stack (ingredients-scoper-stack $ingredients))
  (define $scope-stack (map scoper-scope $scoper-stack))
  (define $scope (apply append $scope-stack))
  (option-bind ($fn $scope) $expressions
    (define $syntax-option (expressions-syntax-option $expressions))
    (define $entry-stack (filter-false (map scoper-entry-option $scoper-stack)))
    (expressions
      (and $syntax-option (entry-stack-do-syntax $entry-stack $syntax-option))
      (expressions-structure $expressions))))

(check-equal?
  (expressions-sexp
    (ingredients-do
      (ingredients
        (expressions #`empty (structure))
        (expressions #`single (structure dynamic-type-a))
        (expressions #`multi (structure dynamic-type-a dynamic-type-b))
        (expressions #`static (structure dynamic-type-a static-type-b dynamic-type-c)))
      (lambda (($scope : Scope))
        (expressions
          (make-syntax
            `(values
              ,@(reverse (filter-false (map binding-identifier-option $scope)))))
          (map binding-type $scope)))))
  (expressions-sexp
    (expressions
      (make-syntax
        `(let-values
          (((tmp-a) single)
            ((tmp-a tmp-b) multi)
            ((tmp-a tmp-c) static))
          (values tmp-a tmp-a tmp-b tmp-a tmp-c)))
      (structure dynamic-type-a dynamic-type-a dynamic-type-b dynamic-type-a static-type-b dynamic-type-c))))

; --------------------------------------------------------------------------------

(define (ingredients-apply-fn
  ($ingredients : Ingredients)
  ($fn : (-> Tuple Expressions))) : Expressions
  (option-ref (ingredients-resolve-fn $ingredients $fn)))

(define (ingredients-gather-fn
  ($ingredients : Ingredients)
  ($fn : (-> Tuple Expressions))) : Expressions
  (option-ref (usage-ingredients-resolve-fn 'direct $ingredients $fn)))

; ----------------------------------------------------------------------------

(define (symbol-ingredients-expressions ($symbol : Symbol) ($ingredients : Ingredients)) : Expressions
  (or
    (case $symbol
      ((type) (resolve-type-ingredients-expressions $ingredients))
      ((word) (resolve-word-ingredients-expressions $ingredients))
      (else #f))
    (ingredients-gather-fn $ingredients
      (lambda (($tuple : Tuple))
        (expressions
          (tuple-syntax-option $tuple)
          (structure (field $symbol (tuple-structure $tuple))))))))

(check-equal?
  (expressions-sexp
    (symbol-ingredients-expressions
      `foo
      (ingredients expressions-a expressions-cd)))
  (expressions-sexp
    (expressions
      #`(let-values (((tmp-c tmp-d) cd)) (vector a tmp-c tmp-d))
      (structure
        (field! `foo dynamic-type-a dynamic-type-c dynamic-type-d)))))

; --------------------------------------------------------------------------

(define (resolve-type-ingredients-expressions ($ingredients : Ingredients)) : (Option Expressions)
  (define $structure (ingredients-structure $ingredients))
  (cond
    ((structure-matches? $structure (structure field-type))
      TODO)
    (else #f)))

; --------------------------------------------------------------------------

(define (resolve-word-ingredients-expressions ($ingredients : Ingredients)) : (Option Expressions)
  (option-bind (single (ingredients-structure $ingredients)) $type
    (and
      (field? $type)
      (null? (field-structure $type))
      (expression-expressions (word-expression (field-symbol $type))))))
; --------------------------------------------------------------------------

(define (ingredients-expressions ($ingredients : Ingredients)) : Expressions
  (ingredients-do $ingredients
    (lambda (($scope : Scope))
      (expressions
        (bind $identifier-list (reverse (scope-identifier-stack $scope))
          (case (length $identifier-list)
            ((0) #f)
            ((1) (make-syntax (car $identifier-list)))
            (else (make-syntax `(values ,@$identifier-list)))))
        (scope-structure $scope)))))

; empty-expression
(check-equal?
  (expressions-sexp
    (ingredients-expressions
      (ingredients
        (expressions #f null-structure))))
  (expressions-sexp
    (expressions #f null-structure)))

; static
(check-equal?
  (expressions-sexp
    (ingredients-expressions
      (ingredients
        (expressions #f (structure static-type-a)))))
  (expressions-sexp
    (expressions #f (structure static-type-a))))

; single-expression
(check-equal?
  (expressions-sexp
    (ingredients-expressions
      (ingredients
        (expressions #`expr (structure dynamic-type-a dynamic-type-b)))))
  (expressions-sexp
    (expressions
      (make-syntax `(let-values (((tmp-a tmp-b) expr)) (values tmp-a tmp-b)))
      (structure dynamic-type-a dynamic-type-b))))

; single-expression & multi-expression
(check-equal?
  (expressions-sexp
    (ingredients-expressions
      (ingredients expressions-a expressions-cd)))
  (expressions-sexp
    (expressions
      #`(let-values (((tmp-a) a) ((tmp-c tmp-d) cd)) (values tmp-a tmp-c tmp-d))
      (structure dynamic-type-a dynamic-type-c dynamic-type-d))))

; ----------------------------------------------------------------------

(define (ingredients-sexp-list ($ingredients : Ingredients)) : (Listof Sexp)
  (reverse (filter-false (map expressions-sexp-option $ingredients))))

(define (tuple-does-ingredients ($scope : Scope) ($ingredients : Ingredients)) : Ingredients
  (ingredients (scope-does-expressions $scope (ingredients-expressions $ingredients))))

(define (ingredients-apply-type ($ingredients : Ingredients)) : Ingredients
  (ingredients
    (ingredients-apply-fn $ingredients
      (lambda (($tuple : Tuple))
        (tuple-expressions
          (map type-expression
            (map expression-type $tuple)))))))

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
