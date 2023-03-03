#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/generate-temporary
  leo/compiler/identifier-utils
  leo/compiler/expression
  leo/compiler/expression-stack-syntax
  leo/compiler/type-utils
  leo/compiler/binding)

(data body
  (syntax-stack : (Stackof Syntax))
  (expression-stack : (Stackof Expression)))

(define null-body (body null null))

; --------------------------------------------------------------

(define (body-append-syntax ($body : Body) ($syntax : Syntax)) : Body
  (body
    (push (body-syntax-stack $body) $syntax)
    (body-expression-stack $body)))

(define (body-append-expression ($body : Body) ($expression : Expression)) : Body
  (body
    (body-syntax-stack $body)
    (push (body-expression-stack $body) $expression)))

(define (body-commit-expression ($body : Body) ($expression : Expression)) : Body
  (cond
    ((expression-identifier? $expression) 
      (body-append-expression $body $expression))
    (else 
      (define $type (expression-type $expression))
      (define $syntax (expression-syntax $expression))
      (define $temporary (type-generate-temporary-option $type))
      (cond
        ($temporary
          (body-append-expression
            (body-append-syntax $body (make-syntax `(define ,$temporary ,$syntax)))
            (expression $temporary $type)))
        (else (body-append-expression $body $expression))))))

(define (body-commit ($body : Body)) : Body
  (fold
    (body (body-syntax-stack $body) null)
    (reverse (body-expression-stack $body))
    body-commit-expression))

(parameterize ((tmp-temporaries? #t))
  (bind $body
    (body-commit
      (body
        (stack syntax-a syntax-b)
        (stack
          (expression syntax-c type-c)
          (expression (make-syntax `(complex)) type-d))))
    (check-equal?
      (map syntax->datum (body-syntax-stack $body))
      (stack `a `b `(define tmp-d (complex))))
    (check-equal?
      (map expression-datum (body-expression-stack $body))
      (stack `c `tmp-d))
    (check-equal?
      (map expression-type (body-expression-stack $body))
      (stack type-c type-d))))

; --------------------------------------------------------------

(define (body-values-syntax-stack ($body : Body)) : (Stackof Syntax)
  (define $syntax-stack (body-syntax-stack $body))
  (define $expression-stack (body-expression-stack $body))
  (push-option
    (body-syntax-stack $body)
    (expression-stack-values-syntax-option $expression-stack)))

(check-equal?
  (map syntax->datum
    (body-values-syntax-stack
      (body
        (stack 
          (make-syntax `(define-values (zero pi) (values 0 3.14)))
          (make-syntax `(define (plus a b) (+ a b))))
        (stack 
          (expression (make-syntax `pi) type-a)
          (expression (make-syntax `(plus pi 2)) type-b)))))
  (stack 
    `(define-values (zero pi) (values 0 3.14))
    `(define (plus a b) (+ a b)) 
    `(values pi (plus pi 2))))
