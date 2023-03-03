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

(define (boolean-expression ($boolean : Boolean)) 
  (expression (make-syntax $boolean) boolean-type))
(define (number-expression ($number : Number)) 
  (expression (make-syntax $number) number-type))
(define (int-expression ($fixnum : Fixnum)) 
  (expression (make-syntax $fixnum) int-type))
(define (float-expression ($flonum : Flonum)) 
  (expression (make-syntax $flonum) float-type))
(define (text-expression ($string : String)) 
  (expression (make-syntax $string) text-type))

(define (expression-dynamic? ($expression : Expression)) : Boolean
  (type-dynamic? (expression-type $expression)))

(define (expression-identifier? ($expression : Expression)) : Boolean
  (identifier? (expression-syntax $expression)))

(define (expression-not-identifier? ($expression : Expression)) : Boolean
  (not (identifier? (expression-syntax $expression))))

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

(define (expression-stack-structure 
  ($expression-stack : (Stackof Expression)))
  : Structure
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
  (expression-stack-structure (stack expression-a expression-b))
  (stack type-a type-b))

(check-equal?
  (expression-stack-syntax-stack (stack expression-a expression-b))
  (stack syntax-a syntax-b))

; ----------------------------------------------------------------------------

(define (syntax-structure-index-expression
  ($syntax : Syntax)
  ($structure : Structure)
  ($index : Exact-Nonnegative-Integer))
  : Expression
  (define $structure-size (structure-size $structure))
  (define $dynamic-index (structure-dynamic-ref $structure $index))
  (define $type (list-ref $structure $index))
  (expression
    (make-syntax
      (and
        $dynamic-index
        (case $structure-size
          ((0) #f)
          ((1) $syntax)
          ((2)
            `(
              ,(if (= $dynamic-index 1) `unsafe-car `unsafe-cdr)
              ,$syntax))
          (else
            `(unsafe-vector-ref 
              ,$syntax
              ,(- $structure-size $dynamic-index 1))))))
    $type))

(define (syntax-structure-expression-stack
  ($syntax : Syntax)
  ($structure : Structure))
  : (Stackof Expression)
  (map 
    (curry (curry syntax-structure-index-expression $syntax) $structure)
    (range (length $structure))))

(check-equal?
  (map
    expression-typed-datum
    (syntax-structure-expression-stack
      syntax-a
      (stack dynamic-type-a dynamic-type-b static-type-c dynamic-type-d)))
  (stack
    (typed `(unsafe-vector-ref a 0) dynamic-type-a)
    (typed `(unsafe-vector-ref a 1) dynamic-type-b)
    (typed `#f static-type-c)
    (typed `(unsafe-vector-ref a 2) dynamic-type-d)))
