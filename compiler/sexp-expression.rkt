#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expression-utils)

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
  `(expression #t (boolean (one (of true false)))))

(check-equal?
  (expression-sexp (sexp-expression 3.14))
  `(expression 3.14 number))

(check-equal?
  (expression-sexp (sexp-expression "foo"))
  `(expression "foo" text))

(check-equal?
  (expression-sexp (sexp-expression `foo))
  `(expression #f foo))

(check-equal?
  (expression-sexp (sexp-expression `(foo 1)))
  `(expression 1 (foo number)))

(check-equal?
  (expression-sexp (sexp-expression `(foo 1 "foo")))
  `(expression (cons 1 "foo") (foo number text)))

(check-equal?
  (expression-sexp (sexp-expression `(foo 1 "foo" 2)))
  `(expression (vector 1 "foo" 2) (foo number text number)))
