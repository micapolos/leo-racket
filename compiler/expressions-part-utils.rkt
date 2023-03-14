#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
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
  leo/compiler/expressions-part-sexp
  leo/compiler/expression
  leo/compiler/sexp-expression
  leo/compiler/expression-utils
  leo/compiler/expression-resolve)

(define (expressions-expressions-part ($expressions : Expressions)) : Expressions-Part
  (or
    (and (null? (expressions-structure $expressions)) null-tuple)
    (option-app tuple (expressions-expression-option $expressions))
    $expressions))

(check-equal?
  (expressions-part-sexp
    (expressions-expressions-part null-expressions))
  `(expressions-part))

(check-equal?
  (expressions-part-sexp
    (expressions-expressions-part expressions-a))
  `(expressions-part 
    (expression a (a racket))))

(check-equal?
  (expressions-part-sexp
    (expressions-expressions-part expressions-ab))
  `(expressions-part 
    (expressions ab (structure (a racket) (b racket)))))

(define (expressions-part-plus-tuple ($expressions-part : Expressions-Part) ($tuple : Tuple)) : Expressions-Part
  (cond
    ((expressions? $expressions-part)
      (expressions-apply-fn $expressions-part
        (lambda (($lhs-tuple : Tuple))
          (tuple-expressions (push-stack $lhs-tuple $tuple)))))
    (else
      (push-stack $expressions-part $tuple))))

(define (expressions-part-plus-expressions
  ($lhs-expressions-part : Expressions-Part) 
  ($rhs-expressions : Expressions)) 
  : Expressions-Part
  (cond
    ((expressions? $lhs-expressions-part)
      (expressions-apply-fn $lhs-expressions-part
        (lambda (($lhs-tuple : Tuple))
          (expressions-apply-fn $rhs-expressions
            (lambda (($rhs-tuple : Tuple))
              (tuple-expressions 
                (push-stack $lhs-tuple $rhs-tuple)))))))
    (else 
      (expressions-apply-fn $rhs-expressions
        (lambda (($rhs-tuple : Tuple))
          (tuple-expressions 
            (push-stack $lhs-expressions-part $rhs-tuple)))))))

(check-equal?
  (expressions-part-sexp
    (expressions-part-plus-expressions expressions-a expressions-b))
  `(expressions-part
    (expressions
      (let-values (((tmp-a) a)) (let-values (((tmp-b) b)) (values tmp-a tmp-b)))
      (structure (a racket) (b racket)))))

(define (expressions-part-plus 
  ($lhs-expressions-part : Expressions-Part) 
  ($rhs-expressions-part : Expressions-Part)) 
  : Expressions-Part
  (cond
    ((expressions? $rhs-expressions-part)
      (cond
        ((expressions? $lhs-expressions-part)
          (expressions-apply-fn $lhs-expressions-part
            (lambda (($lhs-tuple : Tuple))
              (expressions-apply-fn $rhs-expressions-part
                (lambda (($rhs-tuple : Tuple))
                  (tuple-expressions 
                    (push-stack $lhs-tuple $rhs-tuple)))))))
        (else 
          (expressions-apply-fn $rhs-expressions-part
            (lambda (($rhs-tuple : Tuple))
              (tuple-expressions 
                (push-stack $lhs-expressions-part $rhs-tuple)))))))
    (else 
      (expressions-part-plus-tuple $lhs-expressions-part $rhs-expressions-part))))

(define (expressions-part-syntax-stack ($expressions-part : Expressions-Part)) : (Stackof Syntax)
  (filter-false
    (cond
      ((expressions? $expressions-part) 
        (stack (expressions-syntax-option $expressions-part)))
      (else 
        (map expression-syntax-option $expressions-part)))))

(define (expressions-part-structure ($expressions-part : Expressions-Part)) : Structure
  (cond
    ((expressions? $expressions-part) (expressions-structure $expressions-part))
    (else (tuple-structure $expressions-part))))

(define (expressions-part-resolve-fn
  ($expressions-part : Expressions-Part)
  ($fn : (-> Tuple (Option Expressions)))) : (Option Expressions)
  (cond
    ((expressions? $expressions-part) 
      (expressions-resolve-fn $expressions-part $fn))
    (else 
      ($fn $expressions-part))))

; not resolved
(check-equal?
  (option-app expressions-sexp
    (expressions-part-resolve-fn expressions-ab
      (lambda (($tuple : Tuple)) #f)))
  #f)

; resolved to static
(check-equal?
  (option-app expressions-sexp
    (expressions-part-resolve-fn expressions-ab
      (lambda (($tuple : Tuple)) 
        (expressions null-syntax static-structure-a))))
  `(expressions #f (structure a)))

; single-expression
(check-equal?
  (option-app expressions-sexp
    (expressions-part-resolve-fn (tuple expression-a)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions result (structure (a racket))))

; --------------------------------------------------------------------------------

(define (expressions-part-apply-fn
  ($expressions-part : Expressions-Part)
  ($fn : (-> Tuple Expressions))) : Expressions
  (option-ref-or
    (expressions-part-resolve-fn $expressions-part $fn)
    (expressions-part-expressions $expressions-part)))

; --------------------------------------------------------------------------------

(define (expressions-part-do ($expressions-part : Expressions-Part) ($fn : (-> Tuple Expressions))) : Expressions
  (cond
    ((expressions? $expressions-part)
      (expressions-do $expressions-part $fn))
    (else
      (tuple-do $expressions-part $fn))))

; do static
(check-equal?
  (expressions-sexp
    (expressions-part-do expressions-ab
      (lambda (($tuple : Tuple)) 
        (expression-expressions 
          (field-expression `done $tuple)))))
  `(expressions
    (let-values (((tmp-a tmp-b) ab)) (cons tmp-a tmp-b))
    (structure (done (a racket) (b racket)))))

; static-expressions
(check-equal?
  (expressions-sexp
    (expressions-part-do
      (tuple static-expression-a expression-b)
      (lambda (($tuple : Tuple)) 
        (expression-expressions 
          (field-expression `done $tuple)))))
  `(expressions
    (let ((tmp-b (dynamic-b))) tmp-b)
    (structure (done a (b racket)))))

; single-expressions
(check-equal?
  (expressions-sexp
    (expressions-part-do
      (tuple expression-a expression-b)
      (lambda (($tuple : Tuple)) 
        (expression-expressions 
          (field-expression `done $tuple)))))
  `(expressions
    (let ((tmp-a (dynamic-a)) (tmp-b (dynamic-b))) (cons tmp-a tmp-b))
    (structure (done (a racket) (b racket)))))

; mutli-expressions
(check-equal?
  (expressions-sexp
    (expressions-part-do expressions-ab
      (lambda (($tuple : Tuple)) 
        (expression-expressions 
          (field-expression `done $tuple)))))
  `(expressions
    (let-values (((tmp-a tmp-b) ab)) (cons tmp-a tmp-b))
    (structure (done (a racket) (b racket)))))

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
      (tuple expression-a expression-b expression-c)))
  `(expressions
    (vector (dynamic-a) (dynamic-b) (dynamic-c))
    (structure (foo (a racket) (b racket) (c racket)))))

; --------------------------------------------------------------------------

(define (expressions-part-expressions ($expressions-part : Expressions-Part)) : Expressions
  (cond
    ((expressions? $expressions-part) $expressions-part)
    (else (tuple-expressions $expressions-part))))

; single-expression
(check-equal?
  (expressions-sexp
    (expressions-part-expressions
      (tuple expression-a)))
  `(expressions (dynamic-a) (structure (a racket))))

; single-expression & multi-expression
(check-equal?
  (expressions-sexp
    (expressions-part-expressions
      (tuple expression-a expression-b expression-c)))
  `(expressions
    (values (dynamic-a) (dynamic-b) (dynamic-c))
    (structure (a racket) (b racket) (c racket))))

; ----------------------------------------------------------------------

(define (scope-doing-expressions-part ($scope : Scope) ($expressions-part : Expressions-Part)) : Expressions-Part
  (scope-doing-expressions $scope (expressions-part-expressions $expressions-part)))

(define (expressions-part-apply-type ($expressions-part : Expressions-Part)) : Expressions-Part
  (map type-expression (expressions-part-structure $expressions-part)))

(define (expressions-part-apply-racket ($expressions-part : Expressions-Part)) : Expressions-Part
  (expressions 
    (make-syntax 
      `(quote 
        ,(reverse 
          (expressions-part-syntax-stack $expressions-part))))
    (structure (racket))))

(define (expressions-part-lift-structure ($expressions-part : Expressions-Part)) : (Option Structure)
  (structure-lift (expressions-part-structure $expressions-part)))
