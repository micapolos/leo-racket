#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/typed/testing
  leo/compiler/syntax-utils
  leo/compiler/racket
  leo/compiler/srcloc
  leo/compiler/syntax-utils
  leo/compiler/srcloc
  leo/compiler/sourced
  leo/compiler/script)

(define (syntax-line ($syntax : Syntax)) : Line
  (define $srcloc (syntax-srcloc $syntax))
  (define $syntax-e (with-srcloc $srcloc (lambda () (syntax-e $syntax))))
  (unless (not (null? $syntax-e)) (error "null syntax"))
  (with-srcloc
    (syntax-srcloc $syntax)
    (lambda ()
      (cond
        ((symbol? $syntax-e) 
          (phrase $syntax-e null))
        ((list? $syntax-e)
          (define $car (car $syntax-e))
          (unless (identifier? $car) (error "not identifier"))
          (phrase 
            (with-srcloc (syntax-srcloc $car) (lambda () (syntax-e $car)))
            (syntax-list-script (cdr $syntax-e))))
        (else (racket (syntax->datum $syntax)))))))

(define (syntax-list-script ($syntax-list : (Listof Syntax))) : Script
  (map syntax-line (reverse $syntax-list)))

(check-equal?
  (syntax-line (make-syntax srcloc-a 123))
  (racket 123))

(check-equal?
  (syntax-line (make-syntax srcloc-a "foo"))
  (racket "foo"))

(check-equal?
  (syntax-line (make-syntax srcloc-a `foo))
  (phrase `foo null))

(check-equal?
  (syntax-line (make-syntax srcloc-a `(,(make-syntax srcloc-b `foo))))
  (phrase `foo null))

(check-equal?
  (syntax-line 
    (make-syntax srcloc-a `(
      ,(make-syntax srcloc-b `foo)
      ,(make-syntax srcloc-c 1)
      ,(make-syntax srcloc-d "foo"))))
  (phrase `foo
    (stack 
      (racket 1)
      (racket "foo"))))
