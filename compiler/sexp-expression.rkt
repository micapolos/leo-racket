#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/expression-utils)

(define (sexp-expression ($sexp : Sexp)) : Expression
  (cond
    ((number? $sexp) (number-expression $sexp))
    ((string? $sexp) (text-expression $sexp))
    ((symbol? $sexp) (field-expression $sexp))
    ((pair? $sexp) 
      (define $car (car $sexp))
      (define $cdr (cdr $sexp))
      (cond
        ((and (symbol? $car) (list? $cdr))
          (field-expression $car (sexp-list-tuple $cdr)))
        (else (error "dupa"))))
    (else (error "dupa"))))

(define (sexp-list-tuple ($sexp-list : (Listof Sexp))) : Tuple
  (reverse (map sexp-expression $sexp-list)))

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
