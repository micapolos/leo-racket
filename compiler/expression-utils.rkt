#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  racket/list
  leo/typed/base
  leo/typed/option
  leo/typed/stack
  leo/typed/testing
  leo/compiler/racket
  leo/compiler/package
  leo/compiler/expression
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-check
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

(define (expression-typed-sourced ($expression : Expression))
  (typed
    (syntax-sourced (expression-syntax $expression))
    (expression-type $expression)))

(define (expression-sexp ($expression : Expression)) : Sexp
  (syntax->datum (expression-syntax $expression)))

(define (expression-sexp-type ($expression : Expression)) : (Pairof Sexp Type)
  (pair
    (expression-sexp $expression)
    (expression-type $expression)))

(define (tuple-structure 
  ($tuple : Tuple))
  : Structure
  (map expression-type $tuple))

(define (tuple-syntax-stack
  ($tuple : Tuple))
  : (Stackof Syntax)
  (map expression-syntax $tuple))

(define (tuple-dynamic-syntax-stack 
  ($tuple : Tuple))
  : (Stackof Syntax)
  (tuple-syntax-stack
    (filter expression-dynamic? $tuple)))

(check-equal?
  (tuple-structure (stack expression-a expression-b))
  (stack type-a type-b))

(check-equal?
  (tuple-syntax-stack (stack expression-a expression-b))
  (stack syntax-a syntax-b))

; ---------------------------------------------------------

(define (field-expression 
  ($symbol : Symbol)
  ($package : Package)) 
  : Expression
  (expression
    (package-syntax $package)
    (field $symbol (package-structure $package))))

(check-equal?
  (expression-sexp-type
    (field-expression `point
      (package
        syntax-a
        (structure
          dynamic-type-a 
          static-type-b 
          dynamic-type-c))))
  (pair
    `a
    (field `point 
      (stack 
        dynamic-type-a 
        static-type-b 
        dynamic-type-c))))

; ---------------------------------------------------------

(define (expression-field-rhs ($expression : Expression)) : (Option Package)
  (define $type (expression-type $expression))
  (and (field? $type)
    (package
      (expression-syntax $expression) 
      (field-structure $type))))

(check-equal?
  (expression-field-rhs
    (expression syntax-a 
      (field `foo
        (structure type-b type-c))))
  (package syntax-a (structure type-b type-c)))

(check-equal?
  (expression-field-rhs (expression syntax-a (racket `foo)))
  #f)

; ---------------------------------------------------------

(define (expression-apply-tuple
  ($lhs-expression : Expression)
  ($rhs-tuple : Tuple))
  : (Option Package)
  (option-app package
    (make-syntax
      `(,(expression-syntax $lhs-expression)
        ,@(reverse 
          (tuple-dynamic-syntax-stack $rhs-tuple))))
    (type-apply-structure
      (expression-type $lhs-expression)
      (tuple-structure $rhs-tuple))))

(check-equal?
  (option-app package-typed-sexp
    (expression-apply-tuple
      (expression #`fn
        (arrow 
          (structure 
            dynamic-type-a 
            static-type-b 
            dynamic-type-c)
          (structure 
            dynamic-type-c 
            static-type-d)))
      (stack 
        dynamic-expression-a 
        static-expression-b 
        dynamic-expression-c)))
  (typed 
    `(fn a c) 
    (stack 
      dynamic-type-c 
      static-type-d)))

(check-equal?
  (expression-apply-tuple
    (expression #`fn (arrow (structure type-a) (structure type-b)))
    (stack expression-c))
  #f)
