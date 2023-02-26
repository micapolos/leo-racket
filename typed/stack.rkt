#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing)

(define-type (Stackof A) (Listof A))

; ----------------------------------------------------------------------

(define #:forall (A) (stack . ($list : A *)) : (Stackof A)
  (reverse $list))

(check-equal?
  (stack 1 2 3)
  (list 3 2 1))

; ----------------------------------------------------------------------

(define #:forall (A) (push ($stack : (Stackof A)) ($value : A)) : (Stackof A)
  (cons $value $stack))

(define top car)
(define pop cdr)
(define pop-top cadr)
