#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/pretty
  (for-syntax 
    racket/base
    racket/syntax-srcloc))

(define testing? : (Parameter Boolean) (make-parameter #f))

(define-syntax (check-equal? $syntax)
  (syntax-case $syntax ()
    ((_ actual expected)
      (let ((srcloc (syntax-srcloc $syntax))
            (expr (syntax->datum #`(quote actual)))
            (actual-value #`actual)
            (expected-value #`expected))
        #`(parameterize ((testing? #t))
          (check 
            #,srcloc
            #,expr
            #,actual-value
            #,expected-value))))))

(define (check ($srcloc : Any) ($expr : Any) ($actual : Any) ($expected : Any))
  (unless (equal? $actual $expected)
    (error
      (pretty-format 
        `(failure
          (location
            (source ,(srcloc-source (cast $srcloc srcloc)))
            (line ,(srcloc-line (cast $srcloc srcloc)))
            (column ,(srcloc-column (cast $srcloc srcloc))))
          (expression ,$expr)
          (actual ,$actual)
          (expected ,$expected))
        #:mode `write))))

(check-equal? (string-append "foo" "bar") "foobar")
