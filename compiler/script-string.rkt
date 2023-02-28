#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/string
  leo/typed/stack
  leo/typed/testing
  leo/compiler/script
  leo/compiler/racket
  leo/compiler/syntax-utils
  leo/compiler/sourced)

(define (script-string ($script : Script)) : String
  (cond
    ((null? $script) "")
    (else (string-append (script-rhs-string $script) "\n"))))

(define (line-string ($line : Line)) : String
  (define $value (sourced-value $line))
  (cond
    ((racket? $value)
      (define $racket $value) 
      (format "~v" (racket-any $racket)))
    ((phrase? $value)
      (define $phrase $value)
      (define $symbol (sourced-value (phrase-sourced-symbol $phrase)))
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
            (string-indent (string-append "\n" (script-rhs-string $script)))))))))

(define (script-rhs-string ($script : Script)) : String
  (string-join (map line-string (reverse $script)) "\n"))

(define (string-indent ($string : String)) : String
  (string-replace $string "\n" "\n  "))

(check-equal? 
  (line-string (sourced (racket 1) srcloc-a))
  "1")

(check-equal? 
  (line-string (sourced (racket "foo") srcloc-a))
  "\"foo\"")

(check-equal? 
  (line-string (sourced (racket `foo) srcloc-a))
  "'foo")

(check-equal? 
  (line-string (sourced (phrase (sourced `foo srcloc-a) null) srcloc-a))
  "foo")

(check-equal? 
  (line-string 
    (sourced 
      (phrase 
        (sourced `foo srcloc-a) 
        (stack 
          (sourced (racket 1) srcloc-a)))
      srcloc-a))
  "foo 1")

(check-equal? 
  (line-string 
    (sourced 
      (phrase 
        (sourced `foo srcloc-a) 
        (stack 
          (sourced (racket 1) srcloc-a)))
      srcloc-a))
  "foo 1")

(check-equal?
  (line-string 
    (sourced 
      (phrase 
        (sourced `foo srcloc-a) 
        (stack 
          (sourced (racket 1) srcloc-a)
          (sourced (racket 2) srcloc-a)))
      srcloc-a))
  "foo\n  1\n  2")

(check-equal?
  (script-string null)
  "")

(check-equal?
  (script-string 
    (stack
      (sourced 
        (phrase 
          (sourced `foo srcloc-a) 
          (stack 
            (sourced (racket 1) srcloc-a)
            (sourced (racket 2) srcloc-a)))
        srcloc-a)))
  "foo\n  1\n  2\n")
