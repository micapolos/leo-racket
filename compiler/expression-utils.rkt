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
  leo/compiler/sexp-utils
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

(define static-tuple-a (tuple static-expression-a))
(define static-tuple-b (tuple static-expression-b))
(define static-tuple-c (tuple static-expression-c))
(define static-tuple-d (tuple static-expression-d))

(define dynamic-tuple-a (tuple dynamic-expression-a))
(define dynamic-tuple-b (tuple dynamic-expression-b))
(define dynamic-tuple-c (tuple dynamic-expression-c))
(define dynamic-tuple-d (tuple dynamic-expression-d))

(define tuple-a dynamic-tuple-a)
(define tuple-b dynamic-tuple-b)
(define tuple-c dynamic-tuple-c)
(define tuple-d dynamic-tuple-d)

(define tuple-ab (tuple expression-a expression-b))

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

(define (type-expression ($type : Type)) 
  (expression null-syntax (a $type)))

(define (expression-dynamic? ($expression : Expression)) : Boolean
  (type-dynamic? (expression-type $expression)))

(define (expression-identifier? ($expression : Expression)) : Boolean
  (identifier? (expression-syntax $expression)))

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
      (cond
        ((type-dynamic? (expression-type $lhs-expression))
          `(,(expression-syntax $lhs-expression)
            ,@(reverse 
              (tuple-dynamic-syntax-stack $rhs-tuple))))
        (else null-syntax)))
    (type-apply-structure
      (expression-type $lhs-expression)
      (tuple-structure $rhs-tuple))))

(check-equal?
  (option-app package-sexp-structure
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
  (pair 
    `(fn a c) 
    (stack 
      dynamic-type-c 
      static-type-d)))

(check-equal?
  (option-app package-sexp-structure
    (expression-apply-tuple
      (expression #`fn (arrow dynamic-structure-a static-structure-b))
      dynamic-tuple-a))
  (pair null-sexp static-structure-b))

(check-equal?
  (expression-apply-tuple
    (expression #`fn (arrow (structure type-a) (structure type-b)))
    (stack expression-c))
  #f)

; ------------------------------------------------------------------------------

(define (tuple-syntax
  ($tuple : Tuple))
  : Syntax
  (define $dynamic-tuple 
    (filter expression-dynamic? $tuple))
  (define $dynamic-syntax-stack
    (map expression-syntax $dynamic-tuple))
  (define $dynamic-length
    (length $dynamic-syntax-stack))
  (case $dynamic-length
    ((0) (make-syntax #f))
    ((1) (top $dynamic-syntax-stack))
    ((2) 
      (make-syntax
        `(cons 
          ,(pop-top $dynamic-syntax-stack) 
          ,(top $dynamic-syntax-stack))))
    (else 
      (make-syntax
        `(vector 
          ,@(reverse $dynamic-syntax-stack))))))

(check-equal?
  (syntax->datum (tuple-syntax null))
  #f)

(check-equal?
  (syntax->datum (tuple-syntax (stack static-expression-a)))
  #f)

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack dynamic-expression-a)))
  `a)

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack 
        dynamic-expression-a 
        static-expression-a)))
  `a)

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b)))
  `(cons a b))

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        static-expression-c)))
  `(cons a b))

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        dynamic-expression-c)))
  `(vector a b c))

(check-equal?
  (syntax->datum
    (tuple-syntax
      (stack 
        dynamic-expression-a 
        dynamic-expression-b 
        dynamic-expression-c 
        static-expression-d)))
  `(vector a b c))

; -----------------------------------------------------------------

(define (tuple-values-syntax-option ($tuple : Tuple)) : (Option Syntax)
  (define $dynamic-tuple (filter expression-dynamic? $tuple))
  (define $dynamic-syntax-stack (map expression-syntax $dynamic-tuple))
  (make-syntax
    (case (length $dynamic-syntax-stack)
      ((0) #f)
      ((1) (top $dynamic-syntax-stack))
      (else `(values ,@(reverse $dynamic-syntax-stack))))))

(check-equal?
  (option-map
    (tuple-values-syntax-option null)
    syntax->datum)
  #f)

(check-equal?
  (option-map
    (tuple-values-syntax-option
      (stack dynamic-expression-a))
    syntax->datum)
  `a)

(check-equal?
  (option-map
    (tuple-values-syntax-option
      (stack 
        dynamic-expression-a 
        static-expression-b 
        dynamic-expression-c))
    syntax->datum)
  `(values a c))

; ---------------------------------------------------------

(define (field-expression ($symbol : Symbol) ($tuple : Tuple null-tuple)) : Expression
  (expression
    (tuple-syntax $tuple)
    (field $symbol (tuple-structure $tuple))))

(check-equal?
  (expression-sexp-type (field-expression `foo tuple-ab))
  (pair `(cons a b) (field `foo structure-ab)))

; ---------------------------------------------------------

(define (expression-lift-type ($expression : Expression)) : (Option Type)
  (type-lift (expression-type $expression)))

(define (tuple-lift-structure ($tuple : Tuple)) : (Option Structure)
  (structure-lift (tuple-structure $tuple)))
