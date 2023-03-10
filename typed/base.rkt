#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  racket/vector
  (for-syntax 
    racket/base
    racket/string
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

(define (nothing) (error "nothing"))

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

