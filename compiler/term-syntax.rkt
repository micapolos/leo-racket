#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  racket/list
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/generate-temporary
  leo/compiler/term
  (for-syntax racket/base))

; -----------------------------------------------------------------------------

(define-syntax (native! $syntax)
  (syntax-case $syntax ()
    ((_ value)
      #`(native (syntax value)))))

(define-syntax (tuple! $syntax)
  (syntax-case $syntax ()
    ((_ term ...)
      #`(tuple (ann (stack term ...) (Stackof (Term Syntax)))))))

(define-syntax (switch! $syntax)
  (syntax-case $syntax ()
    ((_ term case ...)
      #`(switch term (stack case ...)))))

(define v0 (variable 0))
(define v1 (variable 1))
(define v2 (variable 2))
(define v3 (variable 3))

(define-syntax (lambda! $syntax)
  (syntax-case $syntax ()
    ((_ (symbol ...) term ...)
      #`(abstraction 
        (ann (stack #,@(map (lambda (s) #`(quote #,s)) (syntax-e #`(symbol ...)))) (Stackof Symbol))
        (packet (ann (stack term ...) (Stackof (Term Syntax))))))))

(define-syntax (app! $syntax)
  (syntax-case $syntax ()
    ((_ lhs rhs ...)
      #`(application 
        lhs 
        (ann (stack rhs ...) (Stackof (Term Syntax)))))))

(define-syntax (binding! $syntax)
  (syntax-case $syntax ()
    ((_ (symbol ...) body)
      #`(binding
        (ann 
          (stack #,@(map (lambda (s) #`(quote #,s)) (syntax-e #`(symbol ...))))
          (Stackof Symbol))
        body))))

(define-syntax (let! $syntax)
  (syntax-case $syntax ()
    ((_ (binding ...) body ...)
      #`(binder
        (quote non-recursive)
        (ann 
          (stack #,@(map (lambda (s) #`(binding! #,@s)) (syntax-e #`(binding ...))))
          (Stackof (Binding (Term Syntax))))
        (packet (ann (stack body ...) (Stackof (Term Syntax))))))))

(define-syntax (letrec! $syntax)
  (syntax-case $syntax ()
    ((_ (binding ...) body ...)
      #`(binder
        (quote recursive)
        (ann 
          (stack #,@(map (lambda (s) #`(binding! #,@s)) (syntax-e #`(binding ...))))
          (Stackof (Binding (Term Syntax))))
        (packet (ann (stack body ...) (Stackof (Term Syntax))))))))

; -------------------------------------------------------------------------------------

(define (term-sexp ($term : (Term Syntax))) : Sexp
  (syntax->datum (term-syntax $term)))

(define (term-syntax ($term : (Term Syntax))) : Syntax
  (identifier-stack-term-syntax null $term))

(define (identifier-stack-term-syntax 
  ($identifier-stack : (Stackof Identifier)) 
  ($term : (Term Syntax)))
: Syntax
  (cond
    ((native? $term) 
      (native-value $term))
    ((index? $term)
      (define $value (index-value $term))
      (make-syntax
        (case (index-size $term)
          ((0) (error "index size 0"))
          ((2) (= $value 0))
          (else $value))))
    ((tuple? $term)
      (define $term-stack (tuple-stack $term))
      (define $size (length $term-stack))
      (make-syntax
        (case $size
          ((0) `())
          ((1) 
            (identifier-stack-term-syntax $identifier-stack (car $term-stack)))
          (else
            `(
              ,(if (= $size 2) `cons `vector)
              ,@(reverse
                (map 
                  (curry identifier-stack-term-syntax $identifier-stack)
                  $term-stack)))))))
    ((ref? $term) 
      (define $syntax (identifier-stack-term-syntax $identifier-stack (ref-tuple $term)))
      (define $index (ref-index $term))
      (define $size (index-size $index))
      (define $value (index-value $index))
      (make-syntax
        (case $size
          ((0) (error "impossible"))
          ((1) $syntax)
          ((2) `(,(if (= $value 0) `car `cdr) ,$syntax))
          (else `(vector-ref ,$syntax ,$value)))))
    ((switch? $term) 
      (define $selector-syntax (identifier-stack-term-syntax $identifier-stack (switch-selector $term)))
      (define $case-syntax-stack 
        (map 
          (curry identifier-stack-term-syntax $identifier-stack)
          (switch-case-stack $term)))
      (define $size (length $case-syntax-stack))
      (make-syntax
        (case $size
          ((0) (error "impossible"))
          ((1) (top $case-syntax-stack))
          ((2) 
            `(if ,$selector-syntax 
              ,(pop-top $case-syntax-stack) 
              ,(top $case-syntax-stack)))
          (else 
            `(case ,$selector-syntax
              ,@(map
                  (lambda (($index : Exact-Nonnegative-Integer) ($case-syntax : Syntax))
                    `((,$index) ,$case-syntax))
                  (range (sub1 $size))
                  (reverse (cdr $case-syntax-stack)))
              (else ,(top $case-syntax-stack)))))))
    ((variable? $term)
      (stack-ref $identifier-stack (variable-index $term)))
    ((abstraction? $term)
      (define $tmp-stack 
        (map symbol-temporary (abstraction-symbol-stack $term)))
      (make-syntax
        `(lambda 
          ,(reverse $tmp-stack)
          ,@(identifier-stack-packet-syntax-list
            (push-stack $identifier-stack $tmp-stack)
            (abstraction-packet $term)))))
    ((application? $term)
      (make-syntax
        `(
          ,(identifier-stack-term-syntax 
            $identifier-stack 
            (application-lhs $term))
          ,@(reverse
            (map
              (curry identifier-stack-term-syntax $identifier-stack)
              (application-rhs-stack $term))))))
    ((binder? $term)
      (define $tmp-stack-syntax-pair-stack
        (map 
          (curry identifier-stack-binding-tmp-stack-syntax-pair $identifier-stack)
          (binder-binding-stack $term)))
      (make-syntax
        `(
          ,(case (binder-recursiveness $term)
            ((non-recursive) `let-values)
            ((recursive) `letrec-values))
          ,(reverse
            (map 
              (lambda (($tmp-stack-syntax-pair : (Pairof (Stackof Identifier) Syntax)))
                `(
                  ,(reverse (car $tmp-stack-syntax-pair))
                  ,(cdr $tmp-stack-syntax-pair)))
              $tmp-stack-syntax-pair-stack))
          ,@(identifier-stack-packet-syntax-list
            (push-stack 
              $identifier-stack
              (apply append 
                (map 
                  (ann car (-> (Pairof (Stackof Identifier) Syntax) (Stackof Identifier)))
                  $tmp-stack-syntax-pair-stack)))
            (binder-packet $term)))))))

(define (identifier-stack-binding-tmp-stack-syntax-pair
  ($identifier-stack : (Stackof Identifier))
  ($binding : (Binding (Term Syntax))))
: (Pairof (Stackof Identifier) Syntax)
  (define $tmp-stack (map symbol-temporary (binding-symbol-stack $binding)))
  (pair
    $tmp-stack
    (identifier-stack-term-syntax
      (push-stack $identifier-stack $tmp-stack)
      (binding-body $binding))))

(define (identifier-stack-packet-syntax-list
  ($identifier-stack : (Stackof Identifier))
  ($packet : (Packet (Term Syntax))))
: (Listof Syntax)
  (define $syntax-stack 
    (map
      (curry identifier-stack-term-syntax $identifier-stack)
      (packet-stack $packet)))
  (define $size (length $syntax-stack))
  (case $size
    ((0) null)
    ((1) (list (top $syntax-stack)))
    (else (list (make-syntax `(values ,@(reverse $syntax-stack)))))))

; --------------------------------------------------------------------------------

(check-equal?
  (syntax->datum (term-syntax (native! foo)))
  `foo)

(check-equal? (term-sexp (index 0 1)) 0)

(check-equal? (term-sexp (index 0 2)) #t)
(check-equal? (term-sexp (index 1 2)) #f)

(check-equal? (term-sexp (index 0 3)) 0)
(check-equal? (term-sexp (index 1 3)) 1)
(check-equal? (term-sexp (index 2 3)) 2)

(check-equal?
  (term-sexp (tuple!))
  `())

(check-equal?
  (term-sexp (tuple! (native! a)))
  `a)

(check-equal?
  (term-sexp (tuple! (native! a) (native! b)))
  `(cons a b))

(check-equal?
  (term-sexp (tuple! (native! a) (native! b) (native! c)))
  `(vector a b c))

(check-equal?
  (term-sexp (ref (native! tuple) (index 0 1)))
  `tuple)

(check-equal?
  (term-sexp (ref (native! tuple) (index 0 2)))
  `(car tuple))

(check-equal?
  (term-sexp (ref (native! tuple) (index 1 2)))
  `(cdr tuple))

(check-equal?
  (term-sexp (ref (native! tuple) (index 0 3)))
  `(vector-ref tuple 0))

(check-equal?
  (term-sexp (ref (native! tuple) (index 1 3)))
  `(vector-ref tuple 1))

(check-equal?
  (term-sexp (ref (native! tuple) (index 2 3)))
  `(vector-ref tuple 2))

(check-equal?
  (term-sexp 
    (switch! (native! selector) (native! a)))
  `a)

(check-equal?
  (term-sexp 
    (switch! (native! selector) (native! a) (native! b)))
  `(if selector a b))

(check-equal?
  (term-sexp 
    (switch! (native! selector) (native! a) (native! b) (native! c)))
  `(case selector 
    ((0) a)
    ((1) b)
    (else c)))

(check-equal?
  (term-sexp (app! (native! a)))
  `(a))

(check-equal?
  (term-sexp (app! (native! a) (native! b) (native! c)))
  `(a b c))

(check-equal?
  (term-sexp (lambda! ()))
  `(lambda ()))

(check-equal?
  (term-sexp (lambda! (a) v0))
  `(lambda (tmp-a) tmp-a))

(check-equal?
  (term-sexp (lambda! (a b c) v2))
  `(lambda (tmp-a tmp-b tmp-c) tmp-a))

(check-equal?
  (term-sexp (lambda! (a b c) v2 v1 v0))
  `(lambda (tmp-a tmp-b tmp-c) (values tmp-a tmp-b tmp-c)))

(check-equal?
  (term-sexp (let! ()))
  `(let-values ()))

(check-equal?
  (term-sexp (let! ((() (native! a))) (native! b)))
  `(let-values ((() a)) b))

(check-equal?
  (term-sexp 
    (let! (((a b) (native! term-1))
           ((c d) (native! term-2)))
      v3 v2 v1 v0))
  `(let-values 
    (((tmp-a tmp-b) term-1) 
     ((tmp-c tmp-d) term-2))
    (values tmp-a tmp-b tmp-c tmp-d)))

(check-equal?
  (term-sexp (letrec! ()))
  `(letrec-values ()))

(check-equal?
  (term-sexp (letrec! ((() (native! a))) (native! b)))
  `(letrec-values ((() a)) b))

(check-equal?
  (term-sexp
    (letrec!
      (((a b) (native! term-1))
       ((c d) (native! term-2)))
      v3 v2 v1 v0))
  `(letrec-values 
    (((tmp-a tmp-b) term-1) 
     ((tmp-c tmp-d) term-2))
    (values tmp-a tmp-b tmp-c tmp-d)))
