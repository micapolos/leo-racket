#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing
  (for-syntax racket/base))

(data (V) failure
  (value : V))

(define-syntax (or-failure $syntax)
  (syntax-case $syntax ()
    ((_ body ...)
      #`(with-handlers
        ((exn:fail?
          (lambda (($exn : exn:fail))
            (failure (exn-message $exn)))))
        body ...))))

(check-equal?
  (or-failure (error "dupa"))
  (failure "dupa"))

(check-equal?
  (or-failure 123)
  123)

(define-syntax (check-fail $syntax)
  (syntax-case $syntax ()
    ((_ $expr $value)
      (syntax/loc $syntax
        (check-equal?
          (or-failure $expr)
          (failure $value))))))

(check-fail (error "dupa") "dupa")
