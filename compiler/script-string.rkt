#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/string
  leo/typed/stack
  leo/typed/testing
  leo/compiler/script
  leo/compiler/racket
  leo/compiler/datum-script
  leo/compiler/syntax-utils)

(define (script-string ($script : Script)) : String
  (cond
    ((null? $script) "")
    (else (string-append (script-rhs-string $script) "\n"))))

(define (line-string ($line : Line)) : String
  (define $value $line)
  (cond
    ((racket? $value)
      (define $racket $value) 
      (format "~v" (racket-any $racket)))
    ((phrase? $value)
      (define $phrase $value)
      (define $symbol (phrase-symbol $phrase))
      (define $name (symbol->string $symbol))
      (define $script (phrase-script $phrase))
      (cond
        ((null? $script) $name)
        ((null? (cdr $script))
          (string-append 
            $name
            " " 
            (line-string (car $script))))
        (else 
          (string-append
            $name
            (cond
              ((script-atoms? $script)
                (string-indent 
                  (string-append ": " (script-atoms-rhs-string $script))))
              (else 
                (string-indent 
                  (string-append "\n" (script-rhs-string $script)))))))))))

(define (script-rhs-string ($script : Script)) : String
  (string-join (map line-string (reverse $script)) "\n"))

(define (script-atoms-rhs-string ($script : Script)) : String
  (string-join (map line-string (reverse $script)) " "))

(define (line-atom? ($line : Line)) : Boolean
  (define $value $line)
  (cond
    ((racket? $value) #t)
    ((phrase? $value) (null? (phrase-script $value)))))

(define (script-atoms? ($script : Script)) : Boolean
  (andmap line-atom? $script))

(define (string-indent ($string : String)) : String
  (string-replace $string "\n" "\n  "))

(check-equal? 
  (line-string (datum-line 1))
  "1")

(check-equal? 
  (line-string (datum-line "foo"))
  "\"foo\"")

(check-equal? 
  (line-string (datum-line `foo))
  "foo")

(check-equal? 
  (line-string (datum-line `(foo bar)))
  "foo bar")

(check-equal? 
  (line-string (datum-line `(foo (bar zoo))))
  "foo bar zoo")

(check-equal? 
  (line-string (datum-line `(foo bar)))
  "foo bar")

(check-equal?
  (line-string (datum-line `(foo bar zoo)))
  "foo: bar zoo")

(check-equal?
  (line-string (datum-line `(foo (x 1) (y 2))))
  "foo\n  x 1\n  y 2")

(check-equal?
  (script-string (datum-script `()))
  "")

(check-equal?
  (script-string (datum-script 1))
  "1\n")

(check-equal?
  (script-string (datum-script `(1 2)))
  "1\n2\n")
