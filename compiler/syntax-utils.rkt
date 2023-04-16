#lang leo/typed

(require 
  leo/compiler/sexp-utils)

(define (syntax-sexp ($syntax : Syntax)) : Sexp
  `(syntax ,(syntax->datum $syntax)))

(define (make-syntax ($datum : (Sexpof Syntax)) ($srcloc : (Option srcloc) #f)) : Syntax
  (datum->syntax #f $datum 
    (and $srcloc
      (vector
        (srcloc-source $srcloc)
        (srcloc-line $srcloc)
        (srcloc-column $srcloc)
        (srcloc-position $srcloc)
        (srcloc-span $srcloc)))))

(define (syntax-e-with-srcloc ($syntax : Syntax))
  (with-srcloc (syntax-srcloc $syntax) (lambda () (syntax-e $syntax))))

(define test-syntax (make-syntax `test test-srcloc))

(define syntax-a (make-syntax `a srcloc-a))
(define syntax-b (make-syntax `b srcloc-b))
(define syntax-c (make-syntax `c srcloc-c))
(define syntax-d (make-syntax `d srcloc-d))

(define identifier-a #`a)
(define identifier-b #`b)
(define identifier-c #`c)
(define identifier-d #`d)

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

(define (field-syntax ($syntax : Syntax)) : Syntax
  (cond
    ((symbol? (syntax-e $syntax)) (make-syntax `(,$syntax)))
    (else $syntax)))

(check-equal?
  (syntax->datum (field-syntax (make-syntax `foo)))
  `(foo))

(check-equal?
  (syntax->datum (field-syntax (make-syntax `(foo))))
  `(foo))

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

; ----------------------------------------------------------------------

(define (syntax-stack-values-syntax-option ($syntax-stack : (Stackof Syntax))) : (Option Syntax)
  (make-syntax
    (case (length $syntax-stack)
      ((0) #f)
      ((1) (top $syntax-stack))
      (else `(values ,@(reverse $syntax-stack))))))
