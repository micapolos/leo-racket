#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  racket/list
  leo/typed/stack
  leo/typed/testing
  leo/compiler/expression
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/typed
  leo/compiler/sourced
  leo/compiler/type-utils)

(define dynamic-expression-a (expression syntax-a dynamic-type-a))
(define dynamic-expression-b (expression syntax-b dynamic-type-b))
(define dynamic-expression-c (expression syntax-c dynamic-type-c))
(define dynamic-expression-d (expression syntax-d dynamic-type-d))

(define static-expression-a (expression syntax-a static-type-a))
(define static-expression-b (expression syntax-b static-type-b))
(define static-expression-c (expression syntax-c static-type-c))
(define static-expression-d (expression syntax-d static-type-d))

(define expression-a dynamic-expression-a)
(define expression-b dynamic-expression-b)
(define expression-c dynamic-expression-c)
(define expression-d dynamic-expression-d)

(define (expression-dynamic? ($expression : Expression)) : Boolean
  (type-dynamic? (expression-type $expression)))

(define (expression-identifier? ($expression : Expression)) : Boolean
  (identifier? (expression-syntax $expression)))

(define (expression-typed-sourced ($expression : Expression))
  (typed
    (syntax-sourced (expression-syntax $expression))
    (expression-type $expression)))

(define (expression-datum ($expression : Expression))
  (syntax->datum (expression-syntax $expression)))

(define (expression-typed-datum ($expression : Expression))
  (typed
    (expression-datum $expression)
    (expression-type $expression)))

(define (expression-stack-type-stack 
  ($expression-stack : (Stackof Expression)))
  : (Stackof Type)
  (map expression-type $expression-stack))

(define (expression-stack-syntax-stack
  ($expression-stack : (Stackof Expression)))
  : (Stackof Syntax)
  (map expression-syntax $expression-stack))

(define (expression-stack-dynamic-syntax-stack 
  ($expression-stack : (Stackof Expression)))
  : (Stackof Syntax)
  (expression-stack-syntax-stack
    (filter expression-dynamic? $expression-stack)))

(check-equal?
  (expression-stack-type-stack (stack expression-a expression-b))
  (stack type-a type-b))

(check-equal?
  (expression-stack-syntax-stack (stack expression-a expression-b))
  (stack syntax-a syntax-b))

; ----------------------------------------------------------------------------

(define (syntax-type-stack-index-expression
  ($syntax : Syntax)
  ($type-stack : (Stackof Type))
  ($index : Exact-Nonnegative-Integer))
  : Expression
  (define $type-stack-size (type-stack-size $type-stack))
  (define $dynamic-index (type-stack-dynamic-ref $type-stack $index))
  (define $type (list-ref $type-stack $index))
  (expression
    (make-syntax
      (and
        $dynamic-index
        (case $type-stack-size
          ((0) #f)
          ((1) $syntax)
          ((2)
            `(
              ,(if (= $dynamic-index 1) `unsafe-car `unsafe-cdr)
              ,$syntax))
          (else
            `(unsafe-vector-ref 
              ,$syntax
              ,(- $type-stack-size $dynamic-index 1))))))
    $type))

(define (syntax-type-stack-expression-stack
  ($syntax : Syntax)
  ($type-stack : (Stackof Type)))
  : (Stackof Expression)
  (map 
    (curry (curry syntax-type-stack-index-expression $syntax) $type-stack)
    (range (length $type-stack))))

(check-equal?
  (map
    expression-typed-datum
    (syntax-type-stack-expression-stack
      syntax-a
      (stack dynamic-type-a dynamic-type-b static-type-c dynamic-type-d)))
  (stack
    (typed `(unsafe-vector-ref a 0) dynamic-type-a)
    (typed `(unsafe-vector-ref a 1) dynamic-type-b)
    (typed `#f static-type-c)
    (typed `(unsafe-vector-ref a 2) dynamic-type-d)))
