#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))

(define-syntax (check-equal? $syntax)
  (syntax-case $syntax ()
    ((_ actual expected)
      (quasisyntax
        (let ((actual-value actual)
              (expected-value expected))
          (if (equal? actual-value expected-value) 
            (void)
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
                (unsyntax (syntax-source $syntax))
                (unsyntax (syntax-line $syntax))
                (unsyntax (syntax-column $syntax))
                (quote (unsyntax (syntax->datum (quasisyntax actual))))
                actual-value
                expected-value))))))))
