#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/type
  leo/compiler/type-utils)

(define (sexp-expression ($sexp : Sexp)) : Expression
  (or
    (and (boolean? $sexp) (boolean-expression $sexp))
    (and (number? $sexp) (number-expression $sexp))
    (and (string? $sexp) (text-expression $sexp))
    (and (symbol? $sexp) (field-expression $sexp))
    (and (pair? $sexp) 
      (let ()
        (define $car (car $sexp))
        (define $cdr (cdr $sexp))
        (and (symbol? $car) (list? $cdr)
          (field-expression $car (sexp-list-tuple $cdr)))))
    (racket-expression $sexp)))

(define (sexp-list-tuple ($sexp-list : (Listof Sexp))) : Tuple
  (reverse (map sexp-expression $sexp-list)))

(check-equal?
  (expression-sexp (sexp-expression #t))
  (expression-sexp (boolean-expression #t)))

(check-equal?
  (expression-sexp (sexp-expression 3.14))
  (expression-sexp (number-expression 3.14)))

(check-equal?
  (expression-sexp (sexp-expression "foo"))
  (expression-sexp (text-expression "foo")))

(check-equal?
  (expression-sexp (sexp-expression `foo))
  (expression-sexp (field-expression `foo null-tuple)))

(check-equal?
  (expression-sexp (sexp-expression `(foo 1)))
  (expression-sexp (field-expression! foo (number-expression 1))))

(check-equal?
  (expression-sexp (sexp-expression `(foo 1 "foo")))
  (expression-sexp (expression #`(cons 1 "foo") (field! `foo number-type text-type))))

(check-equal?
  (expression-sexp (sexp-expression `(foo 1 "foo" 2)))
  (expression-sexp (expression #`(vector 1 "foo" 2) (field! `foo number-type text-type number-type))))
