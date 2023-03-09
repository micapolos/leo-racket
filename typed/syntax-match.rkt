#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/testing
  leo/typed/option
  (for-syntax racket/base))

(define (any-syntax (any : Any)) : Syntax
  (if (syntax? any)
    (let ((e (syntax-e any)))
      (datum->syntax #f 
        (cond
          ((symbol? e) e)
          ((boolean? e) e)
          ((number? e) e)
          ((string? e) e)
          ((list? e) (map any-syntax e))
          ((pair? e) (cons (any-syntax (car e)) (any-syntax (cdr e))))
          (else (error (format "Not a syntassx: ~v" e))))
        any))
    (error (error (format "Not a syntax: ~v" any)))))

(define (cast-syntax-any ($any : Any)) : (Syntaxof Any)
  (if (syntax? $any) $any (error (format "not a syntax ~v" $any))))

(define (cast-syntax ($syntax : (Syntaxof Any))) : Syntax
  (let (($ctx $syntax)
        ($srcloc $syntax)
        ($syntax-e (syntax-e $syntax)))
    (cond
      ((symbol? $syntax-e) (datum->syntax $ctx $syntax-e $srcloc))
      ((number? $syntax-e) (datum->syntax $ctx $syntax-e $srcloc))
      ((string? $syntax-e) (datum->syntax $ctx $syntax-e $srcloc))
      ((pair? $syntax-e)
        (datum->syntax 
          $ctx 
          (cons 
            (cast-syntax (cast-syntax-any (car $syntax-e)))
            (cond
              ((list? (cdr $syntax-e)) 
                (map cast-syntax (map cast-syntax-any (cdr $syntax-e))))
              (else (cast-syntax (cast-syntax-any (cdr $syntax-e))))))
          $srcloc))
      (else (error (format "not a syntax ~v" $syntax))))))

(define-type Thunk (Pairof Identifier (Listof Syntax)))

(define (syntax-identifier-option ($syntax : Syntax)) : (Option Identifier)
  (cond
    ((identifier? $syntax) $syntax)
    (else #f)))

(check-equal?
  (option-bind
    (syntax-identifier-option #`foo) $identifier
      (syntax->datum $identifier))
  `foo)

(check-equal?
  (option-bind
    (syntax-identifier-option #`(foo)) $identifier
      (syntax->datum $identifier))
  #f)

(define (syntax-thunk-option ($syntax : Syntax)) : (Option (Syntaxof Thunk))
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((null? $syntax-e) #f)
      ((and (list? $syntax-e) (identifier? (car $syntax-e))) $syntax)
      (else #f))))

(define (syntax-identifier-args? ($syntax : Syntax))
  (let ((e (syntax-e $syntax)))
    (and (list? e) (not (null? e)) (identifier? (car e)))))

(define (syntax-symbol-args? ($syntax : Syntax) ($symbol : Symbol))
  (let ((e (syntax-e $syntax)))
    (and 
      (list? e)
      (not (null? e))
      (identifier? (car e))
      (equal? (syntax-e (car e)) $symbol))))

(define (syntax-symbol-arg? ($syntax : Syntax) ($symbol : Symbol))
  (let ((e (syntax-e $syntax)))
    (and 
      (list? e)
      (not (null? e))
      (identifier? (car e))
      (equal? (syntax-e (car e)) $symbol)
      (not (null? (cdr e)))
      (null? (cddr e)))))

(define (syntax-symbol-arg-arg? ($syntax : Syntax) ($symbol : Symbol))
  (let ((e (syntax-e $syntax)))
    (and 
      (list? e)
      (not (null? e))
      (identifier? (car e))
      (equal? (syntax-e (car e)) $symbol)
      (not (null? (cdr e)))
      (not (null? (cdr (cdr e))))
      (null? (cdddr e)))))

(define (syntax-symbol-arg-args? ($syntax : Syntax) ($symbol : Symbol))
  (let ((e (syntax-e $syntax)))
    (and 
      (list? e)
      (not (null? e))
      (identifier? (car e))
      (equal? (syntax-e (car e)) $symbol)
      (not (null? (cdr e)))
      (not (null? (cdr (cdr e)))))))

(define-syntax
  (syntax-match-symbol-args $syntax)
    (syntax-case $syntax ()
      ((_ syntax symbol args body ...)
        (let ((e (car (generate-temporaries `(e)))))
          #`(let ((#,e (syntax-e syntax)))
            (and
              (list? #,e)
              (>= (length #,e) 1)
              (let ((symbol (car #,e))
                    (args (cdr #,e))) 
                body ...)))))))

(define-syntax
  (syntax-symbol-match-args $syntax)
    (syntax-case $syntax ()
      ((_ syntax symbol args body ...)
        (let ((e (car (generate-temporaries `(e)))))
          #`(let ((#,e (syntax-e syntax)))
            (and
              (list? #,e)
              (>= (length #,e) 1)
              (equal? (syntax-e (car #,e)) symbol)
              (let ((args (cdr #,e))) body ...)))))))

(check-equal?
  (syntax-symbol-match-args #`(foo) `foo args (map syntax->datum args))
  `())

(check-equal?
  (syntax-symbol-match-args #`(foo 1 2) `foo args (map syntax->datum args))
  `(1 2))

(check-equal?
  (syntax-symbol-match-args #`(foo 1 2) `bar args (map syntax->datum args))
  #f)

(define-syntax
  (syntax-symbol-match-arg $syntax)
    (syntax-case $syntax ()
      ((_ syntax symbol arg body ...)
        (let ((e (car (generate-temporaries `(e)))))
          #`(let ((#,e (syntax-e syntax)))
            (and
              (list? #,e)
              (= (length #,e) 2)
              (equal? (syntax-e (car #,e)) symbol)
              (let ((arg (cadr #,e)))
                body ...)))))))

(check-equal?
  (syntax-symbol-match-arg #`(foo 1) `foo lhs (syntax->datum lhs))
  1)

(check-equal?
  (syntax-symbol-match-arg #`(foo 1) `bar lhs (syntax->datum lhs))
  #f)

(check-equal?
  (syntax-symbol-match-arg #`(foo 1 2) `foo lhs (syntax->datum lhs))
  #f)

(define-syntax
  (syntax-symbol-match-arg-arg $syntax)
    (syntax-case $syntax ()
      ((_ syntax symbol lhs rhs body ...)
        (let ((e (car (generate-temporaries `(e)))))
          #`(let ((#,e (syntax-e syntax)))
            (and
              (list? #,e)
              (= (length #,e) 3)
              (equal? (syntax-e (car #,e)) symbol)
              (let ((lhs (cadr #,e))
                    (rhs (caddr #,e)))
                body ...)))))))

(check-equal?
  (syntax-symbol-match-arg-arg #`(foo 1 2) `foo lhs rhs
    (list (syntax->datum lhs) (syntax->datum rhs)))
  (list 1 2))

(check-equal?
  (syntax-symbol-match-arg-arg #`(foo 1 2) `bar lhs rhs
    (list (syntax->datum lhs) (syntax->datum rhs)))
  #f)

(check-equal?
  (syntax-symbol-match-arg-arg #`(foo 1 2 3) `foo lhs rhs
    (list (syntax->datum lhs) (syntax->datum rhs)))
  #f)

(define-syntax
  (syntax-symbol-match-args-arg $syntax)
    (syntax-case $syntax ()
      ((_ syntax symbol args arg body ...)
        (let ((e (car (generate-temporaries `(e))))
              (rev (car (generate-temporaries `(rev)))))
          #`(let ((#,e (syntax-e syntax)))
            (and
              (list? #,e)
              (>= (length #,e) 2)
              (equal? (syntax-e (car #,e)) symbol)
              (let* ((#,rev (reverse (cdr #,e)))
                     (args (reverse (cdr #,rev)))
                     (arg (car #,rev)))
                body ...)))))))

(check-equal?
  (syntax-symbol-match-args-arg #`(foo 1 2 3) `foo args arg 
    (cons (syntax->datum arg) (map syntax->datum args)))
  `(3 1 2))

(check-equal? 
  (option-bind
    (syntax-thunk-option #`(foo 1 2)) $thunk
      (syntax->datum $thunk))
  `(foo 1 2))

(check-equal? 
  (option-bind
    (syntax-thunk-option #`123) $thunk
      (syntax->datum $thunk))
  #f)

(check-equal? (syntax->datum (cast-syntax #`foo)) `foo)
(check-equal? (syntax->datum (cast-syntax #`1)) 1)
(check-equal? (syntax->datum (cast-syntax #`"foo")) "foo")
(check-equal? (syntax->datum (cast-syntax #`(point 1 2))) `(point 1 2))

(define (cast-syntaxes ($list : (Listof Any))) : (Listof Syntax)
  (map cast-syntax (map cast-syntax-any $list)))

(define 
  #:forall (T)
  (syntax-match-list 
    ($syntax : Syntax) 
    ($fn : (-> (Listof Syntax) (Option T)))) : (Option T)
  (let (($syntax-e (syntax-e $syntax)))
    (if (list? $syntax-e) ($fn $syntax-e) #f)))

(check-equal?
  (syntax-match-list 
    #`(point 1 "foo")
    (lambda (($syntaxes : (Listof Syntax)))
      (map syntax->datum $syntaxes)))
  `(point 1 "foo"))

(check-equal?
  (syntax-match-list 
    #`123
    (lambda (($syntaxes : (Listof Syntax)))
      (map syntax->datum $syntaxes)))
  #f)

(define 
  #:forall (T)
  (syntax-match-symbol-rhs
    ($syntax : Syntax)
    ($symbol : Symbol)
    ($fn : (-> (Listof Syntax) (Option T)))) : (Option T)
  (let (($syntax-e (syntax-e $syntax)))
    (cond
      ((null? $syntax-e) #f)
      ((list? $syntax-e)
        (let* (($syntaxes (cast-syntaxes $syntax-e))
               ($car (car $syntaxes))
               ($cdr (cdr $syntaxes)))
          (and 
            (equal? (syntax-e $car) $symbol)
            ($fn $cdr))))
      (else #f))))

(check-equal?
  (syntax-match-symbol-rhs #`(point 1 2) `point
    (lambda ((rhs : (Listof Syntax))) 
      (map syntax->datum rhs)))
  `(1 2))
