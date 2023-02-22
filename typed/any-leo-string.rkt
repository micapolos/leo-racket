#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/string
  leo/typed/testing)

(define (any-leo-string ($any : Any)) : String
  (string-append (any-leo-line $any) "\n"))

(define (any-leo-line ($any : Any)) : String
  (cond
    ((null? $any) 
      (any-leo-default-string $any))
    ((list? $any) 
      (let (($car (car $any))
            ($cdr (cdr $any)))
        (cond
          ((symbol? $car)
            (cond 
              ((null? $cdr)
                (string-append "(" (any-leo-default-string $car) ")"))
              ((null? (cdr $cdr))
                (string-append 
                  (any-leo-default-string $car)
                  " " 
                  (any-leo-line (car $cdr))))
              (else 
                (string-append 
                  (any-leo-default-string $car) 
                  (string-indent (string-append "\n" (anys-leo-string $cdr)))))))
          (else (any-leo-default-string $any)))))
    (else (any-leo-default-string $any))))

(define (any-leo-default-string ($any : Any)) : String
  (format "~a" $any))

(define (anys-leo-string ($anys : (Listof Any))) : String
  (string-join (map any-leo-line $anys) "\n"))

(define (string-indent ($string : String)) : String
  (string-replace $string "\n" "\n  "))

(check-equal? (any-leo-string `()) "()\n")
(check-equal? (any-leo-string 1) "1\n")
(check-equal? (any-leo-string 1.0) "1.0\n")
(check-equal? (any-leo-string "\"foo\"") "\"foo\"\n")
(check-equal? (any-leo-string #t) "#t\n")

(check-equal? (any-leo-string `foo) "foo\n")
(check-equal? (any-leo-string `(foo)) "(foo)\n")
(check-equal? (any-leo-string `(foo a)) "foo a\n")
(check-equal? (any-leo-string `(foo a b)) "foo\n  a\n  b\n")
(check-equal? (any-leo-string `(foo (a b))) "foo a b\n")
