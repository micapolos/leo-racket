#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  (for-syntax racket/base))

(define-type (Maybe A) (Option (Just A)))

(struct (A) just ((value : A))
  #:transparent
  #:type-name Just)

(define-syntax (maybe-bind $syntax)
  (syntax-case $syntax ()
    ((_ arg expr body ...)
      (let (($maybe (car (generate-temporaries `(maybe)))))
        #`(let ((#,$maybe expr))
          (cond
            ((just? #,$maybe) 
              (let ((arg (just-value #,$maybe))) body ...))
            (else #f)))))))

(define-syntax (maybe-map $syntax)
  (syntax-case $syntax ()
    ((_ arg expr body ...)
      (let (($maybe (car (generate-temporaries `(maybe)))))
        #`(let ((#,$maybe expr))
          (cond
            ((just? #,$maybe) 
              (just (let ((arg (just-value #,$maybe))) body ...)))
            (else #f)))))))

(check-equal? 
  (maybe-bind $number (just 123) (just (+ $number 1)))
  (just 124))

(check-equal? 
  (maybe-bind $number #f (just (+ $number 1)))
  #f)

(check-equal? 
  (maybe-map $number (just 123) (+ $number 1))
  (just 124))

(check-equal? 
  (maybe-map $number #f (+ $number 1))
  #f)
