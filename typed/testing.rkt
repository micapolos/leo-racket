#lang typed/racket/base

(provide (all-defined-out))

(require 
  (for-syntax 
    racket/base
    racket/syntax-srcloc))

(define-syntax (check-equal? $syntax)
  (syntax-case $syntax ()
    ((_ actual expected)
      (let ((srcloc (syntax-srcloc $syntax))
            (expr (syntax->datum #`(quote actual)))
            (actual-value #`actual)
            (expected-value #`expected))
        #`(check 
          #,srcloc
          #,expr
          #,actual-value
          #,expected-value)))))

(define (check ($srcloc : Any) ($expr : Any) ($actual : Any) ($expected : Any))
  (unless (equal? $actual $expected)
    (error 
      (format 
        (string-append
          "------------------\n"
          "FAILURE\n"
          "location:   ~a:~a:~a\n"
          "expression: ~s\n"
          "actual:     ~v\n"
          "expected:   ~v\n"
          "------------------"
          )
        (srcloc-source (cast $srcloc srcloc))
        (srcloc-line (cast $srcloc srcloc))
        (srcloc-column (cast $srcloc srcloc))
        $expr
        $actual
        $expected))))

(check-equal? (+ 1 2 ) 3)
