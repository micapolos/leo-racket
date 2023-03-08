#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/compiler/sexp-utils
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/type-utils
  leo/compiler/type
  leo/compiler/typed)

(define (syntax-expression-option ($syntax : Syntax)) : (Option Expression)
  (define $syntax-e (syntax-e $syntax))
  (or
    (and (number? $syntax-e) (number-expression $syntax-e))
    (and (string? $syntax-e) (text-expression $syntax-e))
    (and 
      (list? $syntax-e) 
      (= (length $syntax-e) 2)
      (do
        (define $lhs (syntax-e (car $syntax-e)))
        (define $rhs (syntax-e (cadr $syntax-e)))
        (or
          (and (equal? $lhs `int) (fixnum? $rhs) (int-expression $rhs))
          (and (equal? $lhs `float) (flonum? $rhs) (float-expression $rhs))
          (and (equal? $lhs `boolean) 
            (or
              (and (equal? $rhs `true) (boolean-expression #t))
              (and (equal? $rhs `false) (boolean-expression #f)))))))))

(check-equal?
  (option-map 
    (syntax-expression-option (make-syntax 3.14)) 
    expression-sexp-type)
  (pair 3.14 number-type))

(check-equal?
  (option-map 
    (syntax-expression-option (make-syntax `(int 1)))
    expression-sexp-type)
  (pair 1 int-type))

(check-equal?
  (option-map 
    (syntax-expression-option (make-syntax `(float 3.14)))
    expression-sexp-type)
  (pair 3.14 float-type))

(check-equal?
  (option-map 
    (syntax-expression-option (make-syntax "foo")) 
    expression-sexp-type)
  (pair "foo" text-type))

(check-equal?
  (option-map 
    (syntax-expression-option (make-syntax `(boolean true)))
    expression-sexp-type)
  (pair #t boolean-type))

(check-equal?
  (option-map 
    (syntax-expression-option (make-syntax `(boolean false)))
    expression-sexp-type)
  (pair #f boolean-type))

