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

(define #:forall (A) (push-stack ($stack : (Stackof A)) ($rhs-stack : (Stackof A))) : (Stackof A)
  (append $rhs-stack $stack))

(define top car)
(define pop cdr)
(define pop-top cadr)
(define stack? list?)

(define #:forall (A) (single ($stack : (Stackof A))) : (Option A)
  (and 
    (not (null? $stack))
    (null? (pop $stack))
    (top $stack)))

(check-equal? (single (stack)) #f)
(check-equal? (single (stack 1)) 1)
(check-equal? (single (stack 1 2)) #f)

; --------------------------------------------------------------------

(: option-stack-first (All (V) (-> (Stackof V) (Option V))))
(define (option-stack-first $stack)
  (and
    (not (null? $stack))
    (or
      (top $stack)
      (option-stack-first (pop $stack)))))
