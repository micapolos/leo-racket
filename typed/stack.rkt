#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/testing)

(define-type (Stackof A) (Listof A))
(define-type (Non-Empty-Stackof V) (Pairof V (Listof V)))

; ----------------------------------------------------------------------

(define #:forall (A) (stack . ($list : A *)) : (Stackof A)
  (reverse $list))

(check-equal?
  (stack 1 2 3)
  (list 3 2 1))

; ----------------------------------------------------------------------

(define #:forall (A) (non-empty-stack ($value : A) . ($list : A *)) : (Non-Empty-Stackof A)
  (bind $reverse (reverse (pair $value $list))
    (pair (car $reverse) (cdr $reverse))))

(check-equal?
  (non-empty-stack 1 2 3 4)
  (list 4 3 2 1))

; ----------------------------------------------------------------------

(define #:forall (A) (push ($stack : (Stackof A)) ($value : A)) : (Stackof A)
  (cons $value $stack))

(define top car)
(define pop cdr)
(define pop-top cadr)
(define stack? list?)
(define stack-ref list-ref)

(define #:forall (V) (top-option ($stack : (Stackof V))) : (Option V)
  (cond
    ((null? $stack) #f)
    (else (top $stack))))

(define #:forall (V) (stack-ref-default 
  ($stack : (Stackof V)) 
  ($index : Exact-Nonnegative-Integer) 
  ($default : V)) : V
  (with-handlers 
    ((exn:fail:contract? (lambda (exn) $default)))
    (list-ref $stack $index)))

(define #:forall (V) (stack-ref-option
  ($stack : (Stackof V))
  ($index : Exact-Nonnegative-Integer)
  ($default : V)) : (Option V)
  (stack-ref-default $stack $index #f))

; --------------------------------------------------------------------

(: option-stack-first (All (V) (-> (Stackof V) (Option V))))
(define (option-stack-first $stack)
  (and
    (not (null? $stack))
    (or
      (top $stack)
      (option-stack-first (pop $stack)))))

; --------------------------------------------------------------------

(define #:forall (A) (push-stack ($stack : (Stackof A)) ($rhs-stack : (Stackof A))) : (Stackof A)
  (append $rhs-stack $stack))

(check-equal?
  (push-stack (stack 1 2 3) (stack 4 5 6))
  (stack 1 2 3 4 5 6))

; --------------------------------------------------------------------

(define #:forall (A) (push-option ($stack : (Stackof A)) ($option : (Option A))) : (Stackof A)
  (or 
    (and $option (push $stack $option)) 
    $stack))

(check-equal?
  (push-option (stack 1 2 3) #f)
  (stack 1 2 3))

(check-equal?
  (push-option (stack 1 2 3) 4)
  (stack 1 2 3 4))
