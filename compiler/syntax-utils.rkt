#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/list
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/sexp-utils
  leo/compiler/sourced
  leo/typed/srcloc)

(define (make-syntax ($datum : (Sexpof Syntax)) ($srcloc : (Option srcloc) #f)) : Syntax
  (datum->syntax #f $datum 
    (and $srcloc
      (vector
        (srcloc-source $srcloc)
        (srcloc-line $srcloc)
        (srcloc-column $srcloc)
        (srcloc-position $srcloc)
        (srcloc-span $srcloc)))))

(define (syntax-sourced ($syntax : Syntax)) : (Sourced Sexp)
  (sourced 
    (syntax->datum $syntax) 
    (syntax-srcloc $syntax)))

(define (syntax-e-with-srcloc ($syntax : Syntax))
  (with-srcloc (syntax-srcloc $syntax) (lambda () (syntax-e $syntax))))

(define null-syntax (make-syntax null-sexp))
(define test-syntax (make-syntax `test test-srcloc))

(define syntax-a (make-syntax `a srcloc-a))
(define syntax-b (make-syntax `b srcloc-b))
(define syntax-c (make-syntax `c srcloc-c))
(define syntax-d (make-syntax `d srcloc-d))

(define complex-syntax-a (make-syntax `(complex-a)))
(define complex-syntax-b (make-syntax `(complex-b)))
(define complex-syntax-c (make-syntax `(complex-c)))
(define complex-syntax-d (make-syntax `(complex-d)))

(define atomic-syntax-a (make-syntax `atomic-a))
(define atomic-syntax-b (make-syntax `atomic-b))
(define atomic-syntax-c (make-syntax `atomic-c))
(define atomic-syntax-d (make-syntax `atomic-d))

(define (syntax-atomic? ($syntax : Syntax)) : Boolean
  (bind $e (syntax-e $syntax)
    (or 
      (null? $e)
      (symbol? $e)
      (boolean? $e)
      (number? $e)
      (string? $e)
      (keyword? $e))))

(check (syntax-atomic? #`()))
(check (syntax-atomic? #`foo))
(check (syntax-atomic? #`#f))
(check (syntax-atomic? #`123))
(check (syntax-atomic? #`"foo"))
(check (syntax-atomic? #`#:foo))
(check-not (syntax-atomic? #`(a . b)))
(check-not (syntax-atomic? #`(foo)))

(define (syntax-complex? ($syntax : Syntax)) : Boolean
  (not (syntax-atomic? $syntax)))

(define (syntax-syntax-list ($syntax : Syntax)) : (Listof Syntax)
  (define $syntax-e (syntax-e $syntax))
  (or (and (list? $syntax-e) $syntax-e) (list $syntax)))

(define (syntax-normalize ($syntax : Syntax)) : Syntax
  (bind $syntax-e (syntax-e $syntax)
    (cond
      ((list? $syntax-e) 
        (bind $normalized-e (map syntax-normalize $syntax-e)
          (if (= (length $normalized-e) 1) 
            (car $normalized-e) 
            (make-syntax `(,@$normalized-e)))))
      (else $syntax))))

(check-equal? 
  (syntax->datum (syntax-normalize #`((1) ((2)) ((3) (3)))))
  `(1 2 (3 3)))

; ------------------------------------------------------------------------

(define (syntax-switch-syntax-stack
  ($syntax : Syntax)
  ($syntax-stack : (Stackof Syntax))) : Syntax
  (case (length $syntax-stack)
    ((0) (error "impossible"))
    ((1) (car $syntax-stack))
    ((2) 
      (make-syntax 
        `(if 
          ,$syntax
          ,(pop-top $syntax-stack)
          ,(top $syntax-stack))))
    (else 
      (make-syntax
        `(case ,$syntax
          ,@(map
            (lambda (($index : Exact-Nonnegative-Integer) ($rhs-syntax : Syntax))
              `((,$index) ,$rhs-syntax))
            (range (sub1 (length $syntax-stack)))
            (reverse (pop $syntax-stack)))
          (else ,(top $syntax-stack)))))))

(check-equal?
  (syntax->datum
    (syntax-switch-syntax-stack
      #`expr
      (stack #`zero)))
  `zero)

(check-equal?
  (syntax->datum
    (syntax-switch-syntax-stack
      #`expr
      (stack #`zero #`one)))
  `(if expr zero one))

(check-equal?
  (syntax->datum
    (syntax-switch-syntax-stack
      #`expr
      (stack #`zero #`one #`two)))
  `(case expr
    ((0) zero)
    ((1) one)
    (else two)))
