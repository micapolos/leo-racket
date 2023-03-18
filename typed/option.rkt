#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/testing
  (for-syntax racket/base))

(define #:forall (T) (option-ref ($any : (Option T))) : T
  (if (equal? $any #f)
    (error "False!!!")
    $any))

(define-syntax (option-or $syntax)
  (syntax-case $syntax ()
    ((_ $option $or ...)
      (let (($tmp (car (generate-temporaries `(tmp)))))
        #`(let (($tmp $option))
          (cond
            ($tmp $tmp)
            (else $or ...)))))))

(check-equal?
  (option-or #f 123)
  123)

(check-equal?
  (option-or 123 128)
  123)

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

(define #:forall (V) (option-unsafe-ref ($option : (Option V))) : V
  (unless $option (error "empty option"))
  $option)

(define-syntax (option-app $syntax)
  (syntax-case $syntax ()
    ((_ fn arg ...)
      (let* (($args (syntax-e #`(arg ...)))
             ($tmps (generate-temporaries $args))
             ($let-entries (map list $tmps $args)))
        #`(let (#,@$let-entries)
          (and #,@$tmps (fn #,@$tmps)))))))

(check-equal? (option-app +) 0)
(check-equal? (option-app + 1 2 3) 6)
(check-equal? (option-app + 1 #f 3) #f)
