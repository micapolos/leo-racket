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
  (for-syntax racket/base))

(define-type Term 
  (U
    Syntax
    Index
    Tuple
    Tuple-Ref
    Switch
    Variable
    Abstraction
    Application
    Binder))

(data index
  (value : Exact-Nonnegative-Integer)
  (size : Exact-Nonnegative-Integer))

(data tuple
  (term-stack : (Stackof Term)))

(data tuple-ref
  (term : Term)
  (index : Index))

(data switch
  (term : Term)
  (case-stack : (Stackof Term)))

(data variable
  (index : Exact-Nonnegative-Integer))

(data abstraction
  (symbol-stack : (Stackof Symbol))
  (body : Term))

(data application
  (lhs : Term)
  (rhs-stack : (Stackof Term)))

(data binding
  (symbol-stack : (Stackof Symbol))
  (body : Term))

(data binder
  (recursiveness : Recursiveness)
  (binding-stack : (Stackof Binding))
  (body : Term))

(define-type Recursiveness 
  (U 
    'non-recursive 
    'recursive))

; -----------------------------------------------------------------------------

(define-syntax (tuple! $syntax)
  (syntax-case $syntax ()
    ((_ term ...)
      #`(tuple (stack term ...)))))

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
    ((_ (symbol ...) term)
      #`(abstraction 
        (stack #,@(map (lambda (s) #`(quote #,s)) (syntax-e #`(symbol ...))))
        term))))

(define-syntax (app! $syntax)
  (syntax-case $syntax ()
    ((_ lhs rhs ...)
      #`(application 
        lhs 
        (stack rhs ...)))))

(define-syntax (binding! $syntax)
  (syntax-case $syntax ()
    ((_ (symbol ...) body)
      #`(binding
        (stack #,@(map (lambda (s) #`(quote #,s)) (syntax-e #`(symbol ...))))
        body))))

(define-syntax (let! $syntax)
  (syntax-case $syntax ()
    ((_ (binding ...) body)
      #`(binder
        (quote non-recursive)
        (stack #,@(map (lambda (s) #`(binding! #,@s)) (syntax-e #`(binding ...))))
        body))))

(define-syntax (letrec! $syntax)
  (syntax-case $syntax ()
    ((_ (binding ...) body)
      #`(binder
        (quote recursive)
        (stack #,@(map (lambda (s) #`(binding! #,@s)) (syntax-e #`(binding ...))))
        body))))

; -------------------------------------------------------------------------------------

(define (term-sexp ($term : Term)) : Sexp
  (syntax->datum (term-syntax $term)))

(define (term-syntax ($term : Term)) : Syntax
  (identifier-stack-term-syntax null $term))

(define (identifier-stack-term-syntax 
  ($identifier-stack : (Stackof Identifier)) 
  ($term : Term)) 
: Syntax
  (cond
    ((syntax? $term) $term)
    ((index? $term)
      (define $value (index-value $term))
      (make-syntax
        (case (index-size $term)
          ((0) (error "impossible"))
          ((2) (= $value 0))
          (else $value))))
    ((tuple? $term)
      (define $term-stack (tuple-term-stack $term))
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
    ((tuple-ref? $term) 
      (define $syntax (identifier-stack-term-syntax $identifier-stack (tuple-ref-term $term)))
      (define $index (tuple-ref-index $term))
      (define $size (index-size $index))
      (define $value (index-value $index))
      (make-syntax
        (case $size
          ((0) (error "impossible"))
          ((1) $syntax)
          ((2) `(,(if (= $value 0) `car `cdr) ,$syntax))
          (else `(vector-ref ,$syntax ,$value)))))
    ((switch? $term) 
      (define $selector-syntax (identifier-stack-term-syntax $identifier-stack (switch-term $term)))
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
          ,(identifier-stack-term-syntax
            (push-stack $identifier-stack $tmp-stack)
            (abstraction-body $term)))))
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
          ,(identifier-stack-term-syntax
            (push-stack 
              $identifier-stack
              (apply append 
                (map 
                  (ann car (-> (Pairof (Stackof Identifier) Syntax) (Stackof Identifier)))
                  $tmp-stack-syntax-pair-stack)))
            (binder-body $term)))))))

(define (identifier-stack-binding-tmp-stack-syntax-pair
  ($identifier-stack : (Stackof Identifier))
  ($binding : Binding))
: (Pairof (Stackof Identifier) Syntax)
  (define $tmp-stack (map symbol-temporary (binding-symbol-stack $binding)))
  (pair
    $tmp-stack
    (identifier-stack-term-syntax
      (push-stack $identifier-stack $tmp-stack)
      (binding-body $binding))))

; --------------------------------------------------------------------------------

(check-equal? (term-sexp (index 0 1)) 0)

(check-equal? (term-sexp (index 0 2)) #t)
(check-equal? (term-sexp (index 1 2)) #f)

(check-equal? (term-sexp (index 0 3)) 0)
(check-equal? (term-sexp (index 1 3)) 1)
(check-equal? (term-sexp (index 2 3)) 2)

(check-equal?
  (syntax->datum (term-syntax #`foo))
  `foo)

(check-equal?
  (term-sexp (tuple!))
  `())

(check-equal?
  (term-sexp (tuple! #`a))
  `a)

(check-equal?
  (term-sexp (tuple! #`a #`b))
  `(cons a b))

(check-equal?
  (term-sexp (tuple! #`a #`b #`c))
  `(vector a b c))

(check-equal?
  (term-sexp (tuple-ref #`tuple (index 0 1)))
  `tuple)

(check-equal?
  (term-sexp (tuple-ref #`tuple (index 0 2)))
  `(car tuple))

(check-equal?
  (term-sexp (tuple-ref #`tuple (index 1 2)))
  `(cdr tuple))

(check-equal?
  (term-sexp (tuple-ref #`tuple (index 0 3)))
  `(vector-ref tuple 0))

(check-equal?
  (term-sexp (tuple-ref #`tuple (index 1 3)))
  `(vector-ref tuple 1))

(check-equal?
  (term-sexp (tuple-ref #`tuple (index 2 3)))
  `(vector-ref tuple 2))

(check-equal?
  (term-sexp 
    (switch! #`selector #`a))
  `a)

(check-equal?
  (term-sexp 
    (switch! #`selector #`a #`b))
  `(if selector a b))

(check-equal?
  (term-sexp 
    (switch! #`selector #`a #`b #`c))
  `(case selector 
    ((0) a)
    ((1) b)
    (else c)))

(check-equal?
  (term-sexp (lambda! (a b c) (app! v2 v1 v0)))
  `(lambda (tmp-a tmp-b tmp-c) (tmp-a tmp-b tmp-c)))

(check-equal?
  (term-sexp 
    (let! (((a b) #`term-1)
           ((c d) #`term-2))
      (app! v3 v2 v1 v0)))
  `(let-values 
    (((tmp-a tmp-b) term-1) 
     ((tmp-c tmp-d) term-2))
    (tmp-a tmp-b tmp-c tmp-d)))

(check-equal?
  (term-sexp 
    (letrec!
      (((a b) #`term-1)
       ((c d) #`term-2))
      (app! v3 v2 v1 v0)))
  `(letrec-values 
    (((tmp-a tmp-b) term-1) 
     ((tmp-c tmp-d) term-2))
    (tmp-a tmp-b tmp-c tmp-d)))
