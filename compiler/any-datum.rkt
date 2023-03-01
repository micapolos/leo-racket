#lang typed/racket/base

(provide (all-defined-out))

(define (any-datum ($any : Any)) : Datum
  (cond
    ((symbol? $any) $any)
    ((number? $any) $any)
    ((string? $any) $any)
    ((boolean? $any) $any)
    ((list? $any) (map any-datum $any))
    (else `(any ...))))
