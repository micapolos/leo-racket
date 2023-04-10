#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  racket/function
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

(define-type (Lazy V) (-> V))

(define-syntax (lazy $syntax)
  (syntax-case $syntax ()
    ((_ body ...)
      #`(lambda () body ...))))

(define #:forall (V) (force ($lazy : (Lazy V))) : V
  ($lazy))

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

(define #:forall (A) (lift-option-list ($list : (Listof (Option A)))) : (Option (Listof A))
  (and
    (andmap (ann identity (-> (Option A) (Option A))) $list)
    (filter-false $list)))

(check-equal?
  (lift-option-list (list))
  (list))

(check-equal?
  (lift-option-list (list 1 2 3))
  (list 1 2 3))

(check-equal?
  (lift-option-list (list 1 #f 3))
  #f)

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

; ----------------------------------------------------------------------------

(define-type (Non-Empty-List V) (Pairof V (Listof V)))

(define #:forall (V) (non-empty-reverse ($list : (Non-Empty-List V))) : (Non-Empty-List V)
  (bind $reverse (reverse $list)
    (pair (car $reverse) (cdr $reverse))))

(check-equal? (non-empty-reverse (list 1)) (list 1))
(check-equal? (non-empty-reverse (list 1 2 3 4)) (list 4 3 2 1))

; ----------------------------------------------------------------------------

(define #:forall (V) (car-option ($list : (Listof V))) : (Option V)
  (cond
    ((null? $list) #f)
    (else (car $list))))

(check-equal? (car-option null) #f)
(check-equal? (car-option (list 1 2 3)) 1)

; ----------------------------------------------------------------------------

(define #:forall (I O) (filter-map-fn ($list : (Listof I)) ($fn : (-> I (Option O)))) : (Listof O)
  (filter-false (map $fn $list)))

(check-equal?
  (filter-map-fn
    (list 1 2 3 4 5 6)
    (lambda (($integer : Integer)) : (Option String)
      (and
        (even? $integer)
        (number->string $integer))))
  (list "2" "4" "6"))

(define-syntax (TODO $syntax)
  (syntax-case $syntax ()
    (_ #`(error "TODO"))))
