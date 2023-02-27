#lang typed/racket/base

(provide (all-defined-out))

(require
  (for-syntax 
    racket/base
    racket/string))

(define-syntax (data syntax)
  (syntax-case syntax ()
    ((_ name fields ...)
      #`(struct name (fields ...) 
        #:transparent 
        #:type-name 
        #,(datum->syntax syntax 
          (string->symbol
            (string-join 
              (map string-titlecase 
                (string-split (symbol->string (syntax->datum #`name)) "-"))
              "")))))))

(define-syntax (bind $syntax)
  (syntax-case $syntax ()
    ((_ var expr body ...)
      #`(let ((var expr)) body ...))))

(define #:forall (A) (filter-false ($list : (Listof (Option A)))) : (Listof A)
  (filter 
    (ann (lambda (x) x) ((Option A) -> (Option A) : #:+ A)) 
    $list))
