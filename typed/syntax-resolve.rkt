#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/function
  leo/typed/type
  leo/typed/typed
  leo/typed/types
  leo/typed/type-match
  leo/typed/syntax-get
  leo/typed/syntax-match
  leo/typed/syntax-type
  leo/typed/syntax-typed
  leo/typed/type-parse
  leo/typed/testing)

(define 
  (symbol-make 
    ($symbol : Symbol)) : Syntax
  (syntax-with-type #`#f (symbol-type $symbol)))

(define
  (symbol-args-resolve 
    ($symbol : Symbol) 
    ($args : (Listof Syntax))) : (Option Syntax)
  (cond
    ((= (length $args) 1)
      (define $arg (car $args))
      (syntax-get $arg (symbol-type $symbol)))
    ((and (equal? $symbol `get) (= (length $args) 2))
      (syntax-get (car $args) (syntax-type (cadr $args))))
    ((and (equal? $symbol `check-equal?) (= (length $args) 2))
      (syntax-with-type
        (datum->syntax #f `(,$symbol ,@$args))
        void-type))
    ((and (equal? $symbol `kind) (= (length $args) 1))
      (error "TODO: kind"))
    (else 
      (symbol-args-resolve-apply $symbol $args))))

(define 
  (symbol-args-make 
    ($symbol : Symbol) 
    ($args : (Listof Syntax))) : Syntax
  (typed-field-syntax $symbol $args))

(define
  (symbol-args-resolve-apply
    ($symbol : Symbol)
    ($args : (Listof Syntax)))
  : (Option Syntax)
  (cond
    ((equal? $symbol `apply)
      (cond
        ((null? $args) 
          (error "apply - no function"))
        (else 
          (define $fn-syntax (car $args))
          (define $fn-args (cdr $args))
          (define $fn-type (syntax-type $fn-syntax))
          (unless (arrow-type? $fn-type)
            (error "apply not a function"))
          (define $param-types (arrow-type-lhs-types $fn-type))
          (define $fn-arg-types (map syntax-type $fn-args))
          (unless (arg-types-match-arrow-type? $fn-arg-types $fn-type)
            (error 
              (format 
                "apply type type mismatch: ~a ~a"
                $fn-type
                $fn-arg-types)))
          (unless (= (length (arrow-type-rhs-types $fn-type)) 1)
            (error "arrow multi-return not supported"))
          (syntax-with-type
            (datum->syntax #f `(#%app ,@$args))
            (car (arrow-type-rhs-types $fn-type))))))
    (else #f)))
