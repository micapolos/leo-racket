#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/string
  leo/typed/testing)

(define (sexp-leo-string ($sexp : Sexp)) : String
  (string-append (sexp-leo-line $sexp) "\n"))

(define (sexp-leo-line ($sexp : Sexp)) : String
  (cond
    ((null? $sexp) 
      (sexp-leo-default-string $sexp))
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
                  (sexp-leo-line (car $cdr))))
              (else 
                (string-append 
                  (sexp-leo-default-string $car) 
                  (string-indent (string-append "\n" (sexps-leo-string $cdr)))))))
          (else (sexp-leo-default-string $sexp)))))
    (else (sexp-leo-default-string $sexp))))

(define (sexp-leo-default-string ($sexp : Sexp)) : String
  (format "~a" $sexp))

(define (sexps-leo-string ($sexps : (Listof Sexp))) : String
  (string-join (map sexp-leo-line $sexps) "\n"))

(define (string-indent ($string : String)) : String
  (string-replace $string "\n" "\n  "))

(check-equal? (sexp-leo-string `()) "()\n")
(check-equal? (sexp-leo-string 1) "1\n")
(check-equal? (sexp-leo-string 1.0) "1.0\n")
(check-equal? (sexp-leo-string "\"foo\"") "\"foo\"\n")
(check-equal? (sexp-leo-string #t) "#t\n")

(check-equal? (sexp-leo-string `foo) "foo\n")
(check-equal? (sexp-leo-string `(foo)) "(foo)\n")
(check-equal? (sexp-leo-string `(foo a)) "foo a\n")
(check-equal? (sexp-leo-string `(foo a b)) "foo\n  a\n  b\n")
(check-equal? (sexp-leo-string `(foo (a b))) "foo a b\n")
