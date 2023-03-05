#lang typed/racket/base

(provide (all-defined-out))

(define (any-sexp ($any : Any)) : Sexp
  (cond
    ((symbol? $any) $any)
    ((number? $any) $any)
    ((string? $any) $any)
    ((boolean? $any) $any)
    ((list? $any) (map any-sexp $any))
    (else `(any ...))))
