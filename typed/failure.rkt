#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/testing
  (for-syntax racket/base))

(data (V) failure
  (value : V))

(define null-failure : (Failure Null)
  (failure null))

(define #:forall (F I) (failure-plus ($failure : (Failure F)) ($item : I)) : (Failure (Pairof I F))
  (failure (pair $item (failure-value $failure))))

(define #:forall (V) (top-failure ($value : V)) : (Failure (List V))
  (failure-plus null-failure $value))

(define-syntax (failure! $syntax)
  (syntax-case $syntax ()
    ((_ $value ...)
      (foldl
        (lambda ($syntax $failure)
          #`(failure-plus #,$failure #,$syntax))
        #`null-failure
        (syntax-e #`($value ...))))))

(check-equal? (failure!) null-failure)
(check-equal? (failure! 1) (failure-plus null-failure 1))
(check-equal? (failure! 1 2) (failure-plus (failure-plus null-failure 1) 2))

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

(define-syntax (fail $syntax)
  (syntax-case $syntax ()
    ((_ $body ...)
      #`(error (format "~s" (failure! $body ...))))))
