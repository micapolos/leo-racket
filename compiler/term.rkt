#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/generate-temporary)

(define-type Term 
  (U
    Syntax
    Variable
    Abstraction
    Application
    Binder))

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

(define-type Recursiveness (U 'non-recursive 'recursive))

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

(check-equal?
  (syntax->datum (term-syntax #`some-syntax))
  `some-syntax)

(check-equal?
  (syntax->datum
    (term-syntax 
      (abstraction 
        (stack `a `b `c) 
        (application (variable 2) (stack (variable 1) (variable 0))))))
  `(lambda (tmp-a tmp-b tmp-c) (tmp-a tmp-b tmp-c)))

(check-equal?
  (syntax->datum
    (term-syntax 
      (binder
        `non-recursive
        (stack
          (binding (stack `a `b) #`term-1)
          (binding (stack `c `d) #`term-2))
        (application 
          (variable 3) 
          (stack
            (variable 2) 
            (variable 1) 
            (variable 0))))))
  `(let-values (((tmp-a tmp-b) term-1) 
                ((tmp-c tmp-d) term-2))
    (tmp-a tmp-b tmp-c tmp-d)))

(check-equal?
  (syntax->datum
    (term-syntax 
      (binder
        `recursive
        (stack
          (binding (stack `a `b) #`term-1)
          (binding (stack `c `d) #`term-2))
        (application 
          (variable 3) 
          (stack
            (variable 2) 
            (variable 1) 
            (variable 0))))))
  `(letrec-values (((tmp-a tmp-b) term-1) 
                ((tmp-c tmp-d) term-2))
    (tmp-a tmp-b tmp-c tmp-d)))
