#lang racket/base

(provide (all-defined-out))

(require
  leo/testing
  (for-syntax racket/base))

; --------------------------------------------------------

(define-syntax (bind $syntax)
  (syntax-case $syntax ()
    ((_ expr name body ...)
      #`(let ((name expr)) body ...))))

(check-equal? 
  (bind (string-append "a" "b") x (string-append x x)) 
  "abab")
