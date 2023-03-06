#lang racket/base

(provide (all-defined-out))

(require
  (for-syntax 
    racket/base
    leo/compiler/leo-eval))

(define-syntax (leo $syntax)
  (syntax-case $syntax ()
    ((_ body ...)
      (let ()
        (define $sexp-list (map syntax->datum (syntax-e #`(body ...))))
        (define $sexp (leo-eval $sexp-list))
        #`(quote #,(datum->syntax $syntax $sexp))))))
