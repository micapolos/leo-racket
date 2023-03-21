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

(define (single-use-ingredients-resolve-fn
  ($single-use? : Boolean)
  ($ingredients : Ingredients)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (define $binder-stack (map (curry single-use-expressions-binder $single-use?) $ingredients))
  (define $tuple-stack (map binder-tuple $binder-stack))
  (define $tuple (apply append $tuple-stack))
  (option-bind ($fn $tuple) $expressions
    (define $syntax (expressions-syntax $expressions))
    (define $entry-stack (filter-false (map binder-entry-option $binder-stack)))
    (define $entry (single $entry-stack))
    (cond
      ((and $entry (equal? $syntax (single (entry-identifier-stack $entry))))
        (expressions (entry-syntax $entry) (expressions-structure $expressions)))
      ((null? $entry-stack) $expressions)
      (else 
        (define $entry-let-syntax-stack (map entry-let-syntax $entry-stack))
        (expressions
          (make-syntax
            `(let-values 
              ,(reverse $entry-let-syntax-stack)
              ,(expressions-syntax $expressions)))
          (expressions-structure $expressions))))))

(define (ingredients-resolve-fn
  ($ingredients : Ingredients)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (single-use-ingredients-resolve-fn #f $ingredients $fn))

(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients 
        (expressions null-syntax (structure static-type-a static-type-b))
        (expressions null-syntax (structure static-type-c static-type-d)))
      (lambda (($tuple : Tuple)) 
        (expressions atomic-syntax-a (structure dynamic-type-a)))))
  `(expressions atomic-a (structure (a racket))))

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
        (expressions atomic-syntax-a (structure dynamic-type-a dynamic-type-b))
        (expressions atomic-syntax-b (structure dynamic-type-c dynamic-type-d)))
      tuple-default-apply-fn))
  `(expressions
    (vector atomic-a atomic-a atomic-b atomic-b)
    (structure (resolved (a racket) (b racket) (c racket) (d racket)))))

(check-equal?
  (option-app expressions-sexp
    (ingredients-resolve-fn
      (ingredients 
        (expressions complex-syntax-a (structure dynamic-type-a dynamic-type-b))
        (expressions complex-syntax-b (structure dynamic-type-c dynamic-type-d)))
      tuple-default-apply-fn))
  `(expressions
    (let-values (((tmp-a tmp-b) (complex-a)) 
                 ((tmp-c tmp-d) (complex-b)))
       (vector tmp-a tmp-b tmp-c tmp-d))
    (structure (resolved (a racket) (b racket) (c racket) (d racket)))))

; --------------------------------------------------------------------------------

(define (ingredients-apply-fn
  ($ingredients : Ingredients)
  ($fn : (-> Tuple Expressions))) : Expressions
  (option-ref (ingredients-resolve-fn $ingredients $fn)))

(define (ingredients-gather-fn
  ($ingredients : Ingredients)
  ($fn : (-> Tuple Expressions))) : Expressions
  (option-ref (single-use-ingredients-resolve-fn #t $ingredients $fn)))

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
  `(expressions
    (vector a cd cd)
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
    (values a cd cd) 
    (structure (a racket) (c racket) (d racket))))

; ----------------------------------------------------------------------

(define (ingredients-sexp-list ($ingredients : Ingredients)) : (Listof Sexp)
  (reverse (filter-false (map expressions-sexp-option $ingredients))))

(define (tuple-doing-ingredients ($tuple : Tuple) ($ingredients : Ingredients)) : Ingredients
  (ingredients (tuple-doing-expressions $tuple (ingredients-expressions $ingredients))))

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
