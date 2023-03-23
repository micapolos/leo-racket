#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/ingredients
  leo/compiler/type
  leo/compiler/type-sexp
  leo/compiler/type-utils
  leo/compiler/syntax-utils
  leo/compiler/expressions
  leo/compiler/expressions-binder
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
    (define $syntax (expressions-syntax $expressions))
    (define $entry-stack (filter-false (map binder-entry-option $binder-stack)))
    (expressions
      (entry-stack-do-syntax $entry-stack $syntax)
      (expressions-structure $expressions))))

(define (ingredients-resolve-fn
  ($ingredients : Ingredients)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (usage-ingredients-resolve-fn 'indirect $ingredients $fn))

(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients 
        (expressions null-syntax (structure static-type-a static-type-b))
        (expressions null-syntax (structure static-type-c static-type-d)))
      (lambda (($tuple : Tuple)) 
        (expressions syntax-a (structure dynamic-type-a)))))
  (expressions-sexp
    (expressions syntax-a (structure dynamic-type-a))))

(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients 
        (expressions null-syntax (structure static-type-a static-type-b))
        (expressions null-syntax (structure static-type-c static-type-d)))
      tuple-default-apply-fn))
  (expressions-sexp
    (expressions null-syntax 
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
  (ingredients-gather-fn $ingredients
    (lambda (($tuple : Tuple))
      (make-expressions
        (tuple-syntax $tuple)
        (structure (field $symbol (tuple-structure $tuple)))))))

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
  (expressions-sexp
    (expressions syntax-a (structure dynamic-type-a))))

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

(define (tuple-does-ingredients ($tuple : Tuple) ($ingredients : Ingredients)) : Ingredients
  (ingredients (tuple-does-expressions $tuple (ingredients-expressions $ingredients))))

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
