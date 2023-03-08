#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/string
  racket/pretty
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils)

(define (sexp-list-string ($sexp-list : (Listof Sexp))) : String
  (cond
    ((null? $sexp-list) "")
    (else (string-append (sexp-list-rhs-string $sexp-list) "\n"))))

(define (sexp-string ($sexp : Sexp)) : String
  (or
    (and (symbol? $sexp) 
      (symbol->string $sexp))
    (and (pair? $sexp)
      (let ()
        (define $symbol (car $sexp))
        (define $list (cdr $sexp))
        (and (symbol? $symbol) (list? $list)
          (cond
            ((null? $list) (symbol->string $symbol))
            ((null? (cdr $list))
              (string-append 
                (symbol->string $symbol)
                " " 
                (sexp-string (car $list))))
            (else 
              (string-append
                (symbol->string $symbol)
                (cond
                  ((sexp-list-atoms? $list)
                    (string-indent 
                      (string-append ": " (sexp-list-atoms-rhs-string $list))))
                  (else 
                    (string-indent 
                      (string-append "\n" (sexp-list-rhs-string $list)))))))))))
    (pretty-format 
      $sexp 
      (if (testing?) 10000 32) 
      #:mode `write)))

(define (sexp-list-rhs-string ($sexp-list : (Listof Sexp))) : String
  (string-join (map sexp-string $sexp-list) "\n"))

(define (sexp-list-atoms-rhs-string ($sexp-list : (Listof Sexp))) : String
  (string-join (map sexp-string $sexp-list) " "))

(define (sexp-atom? ($sexp : Sexp)) : Boolean
  (not (pair? $sexp)))

(define (sexp-list-atoms? ($sexp-list : (Listof Sexp))) : Boolean
  (andmap sexp-atom? $sexp-list))

(define (string-indent ($string : String)) : String
  (string-replace $string "\n" "\n  "))

(check-equal? 
  (sexp-string `("foo" bar))
  "(\"foo\" bar)")

(check-equal? 
  (sexp-string 1)
  "1")

(check-equal? 
  (sexp-string "foo")
  "\"foo\"")

(check-equal? 
  (sexp-string `foo)
  "foo")

(check-equal? 
  (sexp-string `(foo bar))
  "foo bar")

(check-equal? 
  (sexp-string `(foo (bar zoo)))
  "foo bar zoo")

(check-equal? 
  (sexp-string `(foo bar))
  "foo bar")

(check-equal?
  (sexp-string `(foo bar zoo))
  "foo: bar zoo")

(check-equal?
  (sexp-string `(foo (x 1) (y 2)))
  "foo\n  x 1\n  y 2")

(check-equal?
  (sexp-list-string `())
  "")

(check-equal?
  (sexp-list-string `(1))
  "1\n")

(check-equal?
  (sexp-list-string `(1 2))
  "1\n2\n")
