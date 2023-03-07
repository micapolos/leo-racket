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
  leo/compiler/package
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

(define (package-plus-tuple ($package : Package) ($tuple : Tuple)) : Package
  (push-stack $package (map expression-expressions $tuple)))

(define (package-structure ($package : Package)) : Structure
  (apply append (map expressions-structure $package)))

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
  `(expressions result (structure (a racket))))

; single-expression & multi-expression
(check-equal?
  (option-app expressions-sexp
    (package-resolve-fn
      (package expressions-a expressions-cd)
      (lambda (($tuple : Tuple))
        (make-expressions #`result (tuple-structure $tuple)))))
  `(expressions 
    (let-values (((tmp-c tmp-d) cd)) result)
    (structure (a racket) (c racket) (d racket))))

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
    (structure (a racket) (b racket) (c racket) (d racket))))

; --------------------------------------------------------------------------------

(define (package-apply-fn
  ($package : Package)
  ($fn : (-> Tuple Expressions))) : Expressions
  (option-ref (package-resolve-fn $package $fn)))

; --------------------------------------------------------------------------------

(define (package-do ($package : Package) ($fn : (-> Scope Expressions))) : Expressions
  (define $expressions-stack $package)
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
    (package-do
      (package expressions-ab expressions-cd)
      (lambda (($scope : Scope)) 
        (expressions null-syntax static-structure-a))))
  `(expressions #f (structure a)))

; static-expressions
(check-equal?
  (expressions-sexp
    (package-do
      (package static-expressions-a expressions-b)
      (lambda (($scope : Scope))
        (make-expressions #`result (map binding-type $scope)))))
  `(expressions 
    (let-values (((tmp-b) b)) result)
    (structure a (b racket))))

; single-expressions
(check-equal?
  (expressions-sexp
    (package-do
      (package expressions-a expressions-b)
      (lambda (($scope : Scope))
        (make-expressions #`result (map binding-type $scope)))))
  `(expressions 
    (let-values (((tmp-a) a) ((tmp-b) b)) result)
    (structure (a racket) (b racket))))

; mutli-expressions
(check-equal?
  (expressions-sexp
    (package-do
      (package expressions-ab expressions-cd)
      (lambda (($scope : Scope))
        (make-expressions #`result (map binding-type $scope)))))
  `(expressions 
    (let-values (((tmp-a tmp-b) ab)
                 ((tmp-c tmp-d) cd))
        result)
    (structure (a racket) (b racket) (c racket) (d racket))))

; ----------------------------------------------------------------------------

(define (symbol-package-expressions ($symbol : Symbol) ($package : Package)) : Expressions
  (package-apply-fn $package
    (lambda (($tuple : Tuple))
      (make-expressions
        (tuple-syntax $tuple)
        (structure (field $symbol (tuple-structure $tuple)))))))

(check-equal?
  (expressions-sexp
    (symbol-package-expressions
      `foo
      (package expressions-a expressions-cd)))
  `(expressions 
    (let-values (((tmp-c tmp-d) cd))
      (vector a tmp-c tmp-d))
    (structure (foo (a racket) (c racket) (d racket)))))

; --------------------------------------------------------------------------

(define (package-expressions ($package : Package)) : Expressions
  (package-apply-fn $package
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
    (package-expressions
      (package expressions-a)))
  `(expressions a (structure (a racket))))

; single-expression & multi-expression
(check-equal?
  (expressions-sexp
    (package-expressions
      (package expressions-a expressions-cd)))
  `(expressions 
    (let-values (((tmp-c tmp-d) cd)) (values a tmp-c tmp-d))
    (structure (a racket) (c racket) (d racket))))

; ----------------------------------------------------------------------

(define (package-sexp-list ($package : Package)) : (Listof Sexp)
  (reverse (filter-false (map expressions-sexp-option $package))))

(define (package-apply-type ($package : Package)) : Package
  (map expression-expressions
    (map type-expression
      (package-structure $package))))

(define (package-apply-compiled ($package : Package)) : Package
  (package 
    (expression-expressions
      (sexp-expression
        `(compiled
          ,@(package-sexp-list $package))))))
