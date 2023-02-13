#lang typed/racket

(provide (all-defined-out))

(define #:forall (T) (non-false ($any : (U T False))) : T
  (if (equal? $any #f)
    (error "False!!!")
    $any))
