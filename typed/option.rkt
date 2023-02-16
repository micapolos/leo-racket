#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/testing
  (for-syntax racket/base))

(define #:forall (T) (option-ref ($any : (Option T))) : T
  (if (equal? $any #f)
    (error "False!!!")
    $any))

(define #:forall (T R) (option-map 
  ($option : (Option T)) 
  ($fn : (-> T (Option R))))
  : (Option R)
  (and $option ($fn $option)))

(define-syntax (option-bind $syntax)
  (syntax-case $syntax ()
    ((_ expr name body ...) 
      #`(let ((name expr))
        (cond 
          ((equal? name #f) #f)
          (else body ...))))))

(check-equal? (option-bind #f value value) #f)
(check-equal? (option-bind (+ 1 2) value (+ value value)) 6)
