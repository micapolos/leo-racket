#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/script)

(define (sexp-line ($sexp : Sexp)) : Line
  (or
    (and (number? $sexp) 
      (line nothing $sexp))
    (and (string? $sexp) 
      (line nothing $sexp))
    (and (symbol? $sexp) 
      (line nothing (sentence $sexp (script null))))
    (and (pair? $sexp) (symbol? (car $sexp)) (list? (cdr $sexp))
      (line nothing (sentence (car $sexp) (sexp-list-script (cdr $sexp)))))
    (error 
      (format "sexp-line ~s" $sexp))))

(define (sexp-list-script ($sexp-list : (Listof Sexp))) : Script
  (script (reverse (map sexp-line $sexp-list))))

(check-equal? (sexp-line 1) (line nothing 1))

(check-equal? (sexp-line "foo") (line nothing "foo"))

(check-equal? 
  (sexp-line `foo) 
  (line nothing (sentence `foo null-script)))

(check-equal? 
  (sexp-line `(foo 1 "foo")) 
  (line nothing 
    (sentence `foo 
      (script 
        (stack 
          (line nothing 1)
          (line nothing "foo"))))))
