#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/racket
  leo/compiler/syntax-utils
  leo/compiler/sourced
  leo/compiler/script)

(define (syntax-line ($syntax : Syntax)) : Line
  (define $syntax-e (syntax-e $syntax))
  (define $srcloc (syntax-srcloc $syntax))
  (unless (not (null? $syntax-e)) (error "null syntax"))
  (sourced
    (cond
      ((symbol? $syntax-e) 
        (phrase (sourced $syntax-e $srcloc) null))
      ((list? $syntax-e)
        (define $car (car $syntax-e))
        (unless (identifier? $car) (error "not identifier"))
        (phrase 
          (sourced (syntax-e $car) (syntax-srcloc $car))
          (syntax-list-script (cdr $syntax-e))))
      (else (racket (syntax->datum $syntax))))
    (syntax-srcloc $syntax)))

(define (syntax-list-script ($syntax-list : (Listof Syntax))) : Script
  (map syntax-line (reverse $syntax-list)))

(check-equal?
  (syntax-line (make-syntax srcloc-a 123))
  (sourced (racket 123) srcloc-a))

(check-equal?
  (syntax-line (make-syntax srcloc-a "foo"))
  (sourced (racket "foo") srcloc-a))

(check-equal?
  (syntax-line (make-syntax srcloc-a `foo))
  (sourced (phrase (sourced `foo srcloc-a) null) srcloc-a))

(check-equal?
  (syntax-line (make-syntax srcloc-a `(,(make-syntax srcloc-b `foo))))
  (sourced (phrase (sourced `foo srcloc-b) null) srcloc-a))

(check-equal?
  (syntax-line 
    (make-syntax srcloc-a `(
      ,(make-syntax srcloc-b `foo)
      ,(make-syntax srcloc-c 1)
      ,(make-syntax srcloc-d "foo"))))
  (sourced 
    (phrase 
      (sourced `foo srcloc-b) 
      (stack 
        (sourced (racket 1) srcloc-c)
        (sourced (racket "foo") srcloc-d)))
    srcloc-a))
