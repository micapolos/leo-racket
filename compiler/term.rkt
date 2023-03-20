#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/generate-temporary
  (for-syntax racket/base))

(define-type Term 
  (U
    Syntax
    Variable
    Abstraction
    Application
    Binder
    Conditional))

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

(data conditional
  (condition : Term)
  (consequent : Term)
  (alternate : Term))

(define-type Recursiveness 
  (U 
    'non-recursive 
    'recursive))

; -----------------------------------------------------------------------------

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

(define-syntax (if! $syntax)
  (syntax-case $syntax ()
    ((_ condition consequent alternate)
      #`(conditional condition consequent alternate))))

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
            (binder-body $term)))))
    ((conditional? $term)
      (make-syntax
        `(if
          ,(identifier-stack-term-syntax $identifier-stack (conditional-condition $term))
          ,(identifier-stack-term-syntax $identifier-stack (conditional-consequent $term))
          ,(identifier-stack-term-syntax $identifier-stack (conditional-alternate $term)))))))

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

(check-equal?
  (syntax->datum (term-syntax #`some-syntax))
  `some-syntax)

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

(check-equal?
  (term-sexp (if! #`a #`b #`c))
  `(if a b c))
