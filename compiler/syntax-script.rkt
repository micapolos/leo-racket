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
  (define $syntax-e (syntax-e-with-srcloc $syntax))
  (unless (not (null? $syntax-e)) (error "null syntax"))
  (with-srcloc
    (syntax-srcloc $syntax)
    (lambda ()
      (cond
        ((symbol? $syntax-e) 
          (phrase $syntax-e null))
        ((list? $syntax-e)
          (define $car (car $syntax-e))
          (define $car-syntax-e (syntax-e-with-srcloc $car))
          (unless (symbol? $car-syntax-e) (error "not identifier"))
          (phrase 
            $car-syntax-e
            (syntax-list-script (cdr $syntax-e))))
        (else (racket (syntax->datum $syntax)))))))

(define (syntax-list-script ($syntax-list : (Listof Syntax))) : Script
  (map syntax-line (reverse $syntax-list)))

(check-equal?
  (syntax-line (make-syntax 123 srcloc-a))
  (racket 123))

(check-equal?
  (syntax-line (make-syntax "foo" srcloc-a))
  (racket "foo"))

(check-equal?
  (syntax-line (make-syntax `foo srcloc-a))
  (phrase `foo null))

(check-equal?
  (syntax-line (make-syntax `(,(make-syntax `foo srcloc-b)) srcloc-a))
  (phrase `foo null))

(check-equal?
  (syntax-line 
    (make-syntax 
      `(
        ,(make-syntax `foo srcloc-b)
        ,(make-syntax 1 srcloc-c)
        ,(make-syntax "foo" srcloc-d))
      srcloc-a))
  (phrase `foo
    (stack 
      (racket 1)
      (racket "foo"))))
