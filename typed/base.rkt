#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  racket/vector
  racket/syntax-srcloc
  (for-syntax 
    racket/base
    racket/string
    racket/require
    leo/typed/symbol-type-name))

(define-syntax (data syntax)
  (syntax-case syntax (of)
    ((_ (name of vars ...) fields ...)
      #`(struct (vars ...) name (fields ...) 
        #:transparent 
        #:type-name 
        #,(datum->syntax syntax (symbol-type-name (syntax->datum #`name) #t))))
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

(define-syntax (lazy $syntax)
  (syntax-case $syntax ()
    ((_ body ...)
      #`(lambda () body ...))))

(define-syntax (bind $syntax)
  (syntax-case $syntax ()
    ((_ var expr body ...)
      #`(let ((var expr)) body ...))))

(define-syntax (do $syntax)
  (syntax-case $syntax ()
    ((_ body ...)
      #`(let () body ...))))

(define #:forall (A) (filter-false ($list : (Listof (Option A)))) : (Listof A)
  (filter 
    (ann (lambda (x) x) ((Option A) -> (Option A) : #:+ A)) 
    $list))

; -------------------------------------------------------------------------

(: fold (All (A B) (-> A (Listof B) (-> A B A) A)))
(define (fold $initial $list $fn)
  (cond
    ((null? $list) $initial)
    (else (fold ($fn $initial (car $list)) (cdr $list) $fn))))

(check-equal?
  (fold 
    "numbers"
    (list 1 2 3)
    (lambda (($string : String) ($number : Number))
      (string-append $string ", " (number->string $number))))
  "numbers, 1, 2, 3")

; -------------------------------------------------------------------------

(define pair cons)
(define left car)
(define right cdr)

; -------------------------------------------------------------------------

(define (sexp-datum ($sexp : Sexp)) : Datum
  (cond
    ((null? $sexp) $sexp)
    ((symbol? $sexp) $sexp)
    ((keyword? $sexp) $sexp)
    ((boolean? $sexp) $sexp)
    ((number? $sexp) $sexp)
    ((char? $sexp) $sexp)
    ((string? $sexp) $sexp)
    ((vector? $sexp) (vector-map sexp-datum $sexp))
    ((box? $sexp) (box (sexp-datum (unbox $sexp))))
    ((pair? $sexp) (cons (sexp-datum (car $sexp)) (sexp-datum (cdr $sexp))))
    (else (error "impossible"))))

; ---------------------------------------------------------------------------

(define #:forall (A) (single ($list : (Listof A))) : (Option A)
  (and 
    (not (null? $list))
    (null? (cdr $list))
    (car $list)))

(check-equal? (single (list)) #f)
(check-equal? (single (list 1)) 1)
(check-equal? (single (list 1 2)) #f)

(define nil (void))

; ---------------------------------------------------------------------------

(define (syntax-srcloc ($syntax : Syntax)) : srcloc
  (srcloc
    (syntax-source $syntax)
    (syntax-line $syntax)
    (syntax-column $syntax)
    (syntax-position $syntax)
    (syntax-span $syntax)))

; ---------------------------------------------------------------------------

(define (syntax-equal? ($lhs : Syntax) ($rhs : Syntax)) : Boolean
  (define $lhs-e (syntax-e $lhs))
  (define $rhs-e (syntax-e $lhs))
  (and
    (cond
      ((null? $lhs-e) 
        (null? $rhs-e))
      ((pair? $lhs-e) 
        (and 
          (equal? (car $lhs-e) (car $rhs-e))
          (equal? (cdr $lhs-e) (cdr $rhs-e))))
      (else (equal? $lhs-e $rhs-e)))
    (equal? 
      (syntax-srcloc $lhs) 
      (syntax-srcloc $rhs))))

; -----------------------------------------------------------------------------------------------

(define-syntax (local $syntax)
  (syntax-case $syntax ()
    ((_ $module $identifier)
      #`(let () (local-require (only-in $module $identifier)) $identifier))))

(check-equal? (local racket/vector vector-map) vector-map)

; ------------------------------------------------------------------------------------------------

(define-syntax (local-app $syntax)
  (syntax-case $syntax ()
    ((_ $module $identifier $body ...)
      #`(#%app (local $module $identifier) $body ...))))

(check-equal? (local-app racket/unsafe/ops unsafe-fx- 3 2) 1)
