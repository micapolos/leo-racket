#lang racket/base

(provide (all-defined-out))

(require
  leo/testing)

(define (any-apply $fn $args)
  (call-with-values
    (lambda ()
      (call-with-values 
        (lambda () (apply values $args))
        $fn))
    list))

(check-equal?
  (any-apply
    (lambda () 128)
    (list))
  (list 128))

(check-equal?
  (any-apply
    (lambda (a b) 
      (values "foo" (+ a b)))
    (list 1 2))
  (list "foo" 3))
