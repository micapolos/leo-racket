#lang typed/racket

(provide (all-defined-out))

(require leo/testing)

(define #:forall (T) (option-ref ($any : (Option T))) : T
  (if (equal? $any #f)
    (error "False!!!")
    $any))

(define-syntax (option-map $syntax)
  (syntax-case $syntax ()
    ((_ expr name body ...) 
      #`(let ((name expr))
        (cond 
          ((equal? name #f) #f)
          (else body ...))))))

(check-equal? (option-map #f value value) #f)
(check-equal? (option-map (+ 1 2) value (+ value value)) 6)
