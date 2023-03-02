#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/compiler/racket
  leo/compiler/sourced
  leo/compiler/syntax-utils
  leo/compiler/srcloc
  leo/compiler/type
  leo/compiler/type-check
  leo/compiler/type-utils
  leo/compiler/generate-temporary
  leo/compiler/typed
  (for-syntax racket/base))

(define-type Typed-Syntax (Typed Syntax Type))

(define typed-syntax-a (typed syntax-a type-a))
(define typed-syntax-b (typed syntax-b type-b))
(define typed-syntax-c (typed syntax-c type-c))
(define typed-syntax-d (typed syntax-d type-d))

(define-syntax (typed-syntax->typed-sexp $syntax)
  (syntax-case $syntax ()
    ((_ expr)
      #`(bind $typed-syntax expr
        (typed 
          (syntax->datum (typed-value $typed-syntax)) 
          (typed-type $typed-syntax))))))

(define-syntax (typed-syntax->typed-sourced $syntax)
  (syntax-case $syntax ()
    ((_ expr)
      #`(bind $typed-syntax expr
        (typed 
          (syntax-sourced (typed-value $typed-syntax))
          (typed-type $typed-syntax))))))

(define (typed-syntax-dynamic? ($typed-syntax : (Typed Syntax Type))) : Boolean
  (type-dynamic? (typed-type $typed-syntax)))

(define (typed-syntax-stack->typed-syntax
  ($typed-syntax-stack : (Stackof (Typed Syntax Type))))
  : (Typed Syntax Type)
  (unless (= (length $typed-syntax-stack) 1)
    (error "typed-syntax-stack->typed-syntax"))
  (top $typed-syntax-stack))

(define-syntax (typed-stack->type-stack $syntax)
  (syntax-case $syntax ()
    ((_ $typed-stack)
      #`(map 
        (ann typed-type (-> (Typed Any Type) Type))
        $typed-stack))))

(define-syntax (typed-syntax-stack->syntax-stack $syntax)
  (syntax-case $syntax ()
    ((_ $typed-syntax-stack)
      #`(map 
        (ann typed-value (-> (Typed Syntax Any) Syntax))
        $typed-syntax-stack))))

(define-syntax (typed-syntax-stack->dynamic-syntax-stack $syntax)
  (syntax-case $syntax ()
    ((_ $typed-syntax-stack)
      #`(typed-syntax-stack->syntax-stack
        (filter 
          (ann typed-syntax-dynamic? (-> Typed-Syntax Boolean))
          $typed-syntax-stack)))))

(check-equal?
  (typed-stack->type-stack
    (stack typed-syntax-a typed-syntax-b))
  (stack type-a type-b))

; ------------------------------------------------------------------

(define (typed-syntax-stack->syntax
  ($typed-syntax-stack : (Stackof (Typed Syntax Type)))
  ($srcloc : srcloc))
  : Syntax
  (define $dynamic-typed-syntax-stack 
    (filter typed-syntax-dynamic? $typed-syntax-stack))
  (define $dynamic-syntax-stack
    (map (ann typed-value (-> (Typed Syntax Type) Syntax)) $dynamic-typed-syntax-stack))
  (define $dynamic-length 
    (length $dynamic-syntax-stack))
  (case $dynamic-length
    ((0) (make-syntax #f $srcloc))
    ((1) (top $dynamic-syntax-stack))
    ((2) 
      (make-syntax
        `(cons ,(pop-top $dynamic-syntax-stack) ,(top $dynamic-syntax-stack))
        $srcloc))
    (else 
      (make-syntax
        `(vector ,@(reverse $dynamic-syntax-stack))
        $srcloc))))

(check-equal?
  (syntax-sourced
    (typed-syntax-stack->syntax 
      (stack
        (typed test-syntax (field `foo null)))
      test-srcloc))
  (sourced #f test-srcloc))

(check-equal?
  (syntax-sourced
    (typed-syntax-stack->syntax 
      (stack
        (typed syntax-a (field `foo null))
        (typed syntax-b (racket `number)))
      test-srcloc))
  (sourced `b srcloc-b))

(check-equal?
  (syntax-sourced
    (typed-syntax-stack->syntax 
      (stack
        (typed syntax-a (field `foo null))
        (typed syntax-b (racket `number))
        (typed syntax-c (racket `string)))
      test-srcloc))
  (sourced `(cons b c) test-srcloc))

(check-equal?
  (syntax-sourced
    (typed-syntax-stack->syntax 
      (stack
        (typed syntax-a static-type-a)
        (typed syntax-b dynamic-type-b)
        (typed syntax-c dynamic-type-c)
        (typed syntax-d dynamic-type-d))
      test-srcloc))
  (sourced `(vector b c d) test-srcloc))

; ------------------------------------------------------------------

(define (typed-syntax-stack-make
  ($typed-syntax-stack : (Stackof (Typed Syntax Type)))
  ($symbol : Symbol)
  ($srcloc : srcloc))
  : (Typed Syntax Field)
  (typed
    (typed-syntax-stack->syntax 
      $typed-syntax-stack 
      $srcloc)
    (field $symbol 
      (map 
        (ann typed-type (-> (Typed Syntax Type) Type)) 
        $typed-syntax-stack))))

(bind $typed-syntax
  (typed-syntax-stack-make
    (stack
      (typed syntax-a static-type-a)
      (typed syntax-b dynamic-type-b)
      (typed syntax-c dynamic-type-c))
    `tuple
    test-srcloc)
  (check-equal? 
    (typed-type $typed-syntax) 
    (field `tuple (stack static-type-a dynamic-type-b dynamic-type-c)))
  (check-equal? 
    (syntax-sourced (typed-value $typed-syntax))
    (sourced `(cons b c) test-srcloc)))

; -------------------------------------------------------------------

(define (syntax-type-stack-ref
  ($syntax : Syntax)
  ($type-stack : (Stackof Type))
  ($index : Exact-Nonnegative-Integer)
  ($srcloc : srcloc))
  : (Typed Syntax Type)
  (define $type-stack-size (type-stack-size $type-stack))
  (define $dynamic-index (type-stack-dynamic-ref $type-stack $index))
  (typed 
    (make-syntax
      (and
        $dynamic-index
        (case $type-stack-size
          ((0) (error "impossible"))
          ((1) $syntax)
          ((2)
            `(,(if (= $dynamic-index 1) `unsafe-car `unsafe-cdr) ,$syntax))
          (else
            `(unsafe-vector-ref 
              ,$syntax
              ,(- $type-stack-size $dynamic-index 1)))))
       $srcloc)
    (list-ref $type-stack $index)))

(check-equal? 
  (typed-syntax->typed-sourced
    (syntax-type-stack-ref 
      syntax-a
      (stack static-type-a)
      0
      test-srcloc))
  (typed (sourced #f test-srcloc) static-type-a))

(check-equal? 
  (typed-syntax->typed-sourced
    (syntax-type-stack-ref 
      syntax-a
      (stack dynamic-type-a static-type-b)
      0
      test-srcloc)) 
  (typed (sourced #f test-srcloc) static-type-b))

(check-equal? 
  (typed-syntax->typed-sourced
    (syntax-type-stack-ref 
      syntax-a
      (stack dynamic-type-a static-type-b)
      1
      test-srcloc)) 
  (typed (sourced `a srcloc-a) dynamic-type-a))

(check-equal? 
  (typed-syntax->typed-sourced
    (syntax-type-stack-ref 
      syntax-a
      (stack dynamic-type-a static-type-b dynamic-type-c)
      0
      test-srcloc)) 
  (typed (sourced `(unsafe-cdr a) test-srcloc) dynamic-type-c))

(check-equal? 
  (typed-syntax->typed-sourced
    (syntax-type-stack-ref 
      syntax-a
      (stack dynamic-type-a static-type-b dynamic-type-c)
      1
      test-srcloc)) 
  (typed (sourced #f test-srcloc) static-type-b))

(check-equal? 
  (typed-syntax->typed-sourced 
    (syntax-type-stack-ref 
      syntax-a 
      (stack dynamic-type-a static-type-b dynamic-type-c)
      2
      test-srcloc)) 
  (typed (sourced `(unsafe-car a) test-srcloc) dynamic-type-a))

(check-equal? 
  (typed-syntax->typed-sourced
    (syntax-type-stack-ref 
      syntax-a 
      (stack dynamic-type-a dynamic-type-b static-type-c dynamic-type-d)
      0
      test-srcloc)) 
  (typed (sourced `(unsafe-vector-ref a 2) test-srcloc) dynamic-type-d))

(check-equal? 
  (typed-syntax->typed-sourced
    (syntax-type-stack-ref 
      syntax-a 
      (stack dynamic-type-a dynamic-type-b static-type-c dynamic-type-d)
      1
      test-srcloc)) 
  (typed (sourced #f test-srcloc) static-type-c))

(check-equal? 
  (typed-syntax->typed-sourced
    (syntax-type-stack-ref 
      syntax-a 
      (stack dynamic-type-a dynamic-type-b static-type-c dynamic-type-d)
      2
      test-srcloc)) 
  (typed (sourced `(unsafe-vector-ref a 1) test-srcloc) dynamic-type-b))

(check-equal? 
  (typed-syntax->typed-sourced
    (syntax-type-stack-ref 
      syntax-a 
      (stack dynamic-type-a dynamic-type-b static-type-c dynamic-type-d)
      3
      test-srcloc)) 
  (typed (sourced `(unsafe-vector-ref a 0) test-srcloc) dynamic-type-a))

; -------------------------------------------------------------------

(define (typed-syntax-stack-resolve-apply
  ($lhs-typed-syntax-stack : (Stackof (Typed Syntax Type)))
  ($rhs-typed-syntax-stack : (Stackof (Typed Syntax Type)))
  ($srcloc : srcloc))
  : (Option (Typed Syntax Type))
  (and (= (length $lhs-typed-syntax-stack) 1)
    (bind $lhs-typed-syntax (top $lhs-typed-syntax-stack)
      (define $lhs-type (typed-type $lhs-typed-syntax))
      (and (arrow? $lhs-type)
        (let* (($arrow-lhs-type-stack (arrow-lhs-type-stack $lhs-type))
              ($arrow-rhs-type-stack (arrow-rhs-type-stack $lhs-type))
              ($arrow-rhs-type (let ()
                (unless (= (length $arrow-rhs-type-stack) 1) (error "multi-rhs"))
                (car $arrow-rhs-type-stack)))
              ($rhs-type-stack (map 
                (ann typed-type (-> (Typed Syntax Type) Type)) 
                $rhs-typed-syntax-stack)))
          (and (type-stack-check? $rhs-type-stack $arrow-lhs-type-stack)
            (let ()
              (define $lhs-syntax (typed-value $lhs-typed-syntax))
              (define $arg-typed-syntax-stack 
                (filter typed-syntax-dynamic? $rhs-typed-syntax-stack))
              (define $arg-syntax-stack 
                (map (ann typed-value (-> (Typed Syntax Type) Syntax)) 
                $arg-typed-syntax-stack))
              (typed
                (make-syntax `(,$lhs-syntax ,@(reverse $arg-syntax-stack)) $srcloc)
                $arrow-rhs-type))))))))

(bind $option
  (typed-syntax-stack-resolve-apply
    (stack 
      (typed test-syntax
        (arrow 
          (stack dynamic-type-a static-type-b dynamic-type-c)
          (stack dynamic-type-d))))
    (stack 
      (typed syntax-a dynamic-type-a)
      (typed syntax-b static-type-b)
      (typed syntax-c dynamic-type-c))
    test-srcloc)
  (option-bind $option $result
    (check-equal? 
      (syntax->datum (typed-value $result)) 
      `(test a c))
    (check-equal? 
      (typed-type $result) 
      dynamic-type-d)))

(check-equal?
  (typed-syntax-stack-resolve-apply
    (stack (typed syntax-a (arrow (stack dynamic-type-a) (stack dynamic-type-b))))
    (stack (typed syntax-b dynamic-type-c))
    test-srcloc)
  #f)
