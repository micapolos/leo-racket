#lang typed/racket/base

(provide (all-defined-out))

(require
  (for-syntax 
    racket/base
    racket/string
    leo/typed/symbol-type-name))

(define-syntax (data syntax)
  (syntax-case syntax (forall)
    ((_ (vars ...) name fields ...)
      #`(struct (vars ...) name (fields ...) 
        #:transparent 
        #:type-name 
        #,(datum->syntax syntax (symbol-type-name (syntax->datum #`name)))))
    ((_ name fields ...)
      #`(struct name (fields ...) 
        #:transparent 
        #:type-name 
        #,(datum->syntax syntax (symbol-type-name (syntax->datum #`name)))))))

(define-syntax (bind $syntax)
  (syntax-case $syntax ()
    ((_ var expr body ...)
      #`(let ((var expr)) body ...))))

(define #:forall (A) (filter-false ($list : (Listof (Option A)))) : (Listof A)
  (filter 
    (ann (lambda (x) x) ((Option A) -> (Option A) : #:+ A)) 
    $list))
