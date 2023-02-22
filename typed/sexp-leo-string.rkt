#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/string
  leo/typed/testing)

(define (sexp-leo-string ($sexp : Sexp)) : String
  (cond
    ((null? $sexp) "")
    ((list? $sexp) 
      (let (($car (car $sexp))
            ($cdr (cdr $sexp)))
        (cond
          ((symbol? $car)
            (cond 
              ((null? $cdr)
                (string-append "(" (sexp-leo-default-string $car) ")"))
              ((null? (cdr $cdr))
                (string-append 
                  (sexp-leo-default-string $car)
                  " " 
                  (sexp-leo-string (car $cdr))))
              (else 
                (string-append 
                  (sexp-leo-default-string $car) 
                  (indent (string-append "\n" (sexps-leo-string $cdr)))))))
          (else (sexp-leo-default-string $sexp)))))
    (else (sexp-leo-default-string $sexp))))

(define (sexp-leo-default-string ($sexp : Sexp)) : String
  (format "~a" $sexp))

(define (sexps-leo-string ($sexps : (Listof Sexp))) : String
  (string-join (map sexp-leo-string $sexps) "\n"))

(define (indent ($string : String)) : String
  (string-replace $string "\n" "\n  "))

(check-equal? (sexp-leo-string `()) "")
(check-equal? (sexp-leo-string 1) "1")
(check-equal? (sexp-leo-string 1.0) "1.0")
(check-equal? (sexp-leo-string "\"foo\"") "\"foo\"")
(check-equal? (sexp-leo-string #t) "#t")

(check-equal? (sexp-leo-string `foo) "foo")
(check-equal? (sexp-leo-string `(foo)) "(foo)")
(check-equal? (sexp-leo-string `(foo a)) "foo a")
(check-equal? (sexp-leo-string `(foo a b)) "foo\n  a\n  b")
(check-equal? (sexp-leo-string `(foo (a b))) "foo a b")
