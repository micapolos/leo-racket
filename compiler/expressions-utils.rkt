#lang typed/racket/base

(provide (all-defined-out))

(require
  racket/function
  racket/list
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/typed/stack
  leo/compiler/sexp-utils
  leo/compiler/expressions
  leo/compiler/expressions-sexp
  leo/compiler/generate-temporary
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/syntax-utils
  leo/compiler/type
  leo/compiler/type-match
  leo/compiler/typed
  leo/compiler/type-utils)

(define static-expressions-a (expressions null-syntax static-structure-a))

(define expressions-a (expressions #`a structure-a))
(define expressions-b (expressions #`b structure-b))

(define expressions-ab (expressions #`ab structure-ab))
(define expressions-cd (expressions #`cd structure-cd))

(define-type Resolve-Fn (-> Tuple (Option Expressions)))
(define-type Apply-Fn (-> Tuple Expressions))

(define tuple-default-resolve-fn
  (lambda (($tuple : Tuple)) : (Option Expressions)
    (expression-expressions
      (field-expression `resolved $tuple))))

(define tuple-non-resolve-fn
  (lambda (($tuple : Tuple)) : (Option Expressions) #f))

(define tuple-default-apply-fn
  (lambda (($tuple : Tuple)) : Expressions
    (expression-expressions
      (field-expression `resolved $tuple))))

(define (make-expressions ($syntax : Syntax) ($structure : Structure)) : Expressions
  (expressions
    (or (and (structure-dynamic? $structure) $syntax) null-syntax)
    $structure))

(define (expressions-size ($expressions : Expressions)) : Exact-Nonnegative-Integer
  (length (expressions-structure $expressions)))

(define (expression-expressions ($expression : Expression)) : Expressions
  (tuple-expressions (stack $expression)))

(define (tuple-expressions ($tuple : Tuple)) : Expressions
  (expressions
    (or (tuple-values-syntax-option $tuple) null-syntax)
    (tuple-structure $tuple)))

(check-equal?
  (expressions-sexp-structure (tuple-expressions (tuple static-expression-a)))
  (pair null-sexp (structure static-type-a)))

(check-equal?
  (expressions-sexp-structure
    (tuple-expressions
      (tuple dynamic-expression-a static-expression-b)))
  (pair 
    `a 
    (structure dynamic-type-a static-type-b)))

(check-equal?
  (expressions-sexp-structure
    (tuple-expressions
      (tuple dynamic-expression-a static-expression-b dynamic-expression-c)))
  (pair 
    `(values a c) 
    (structure dynamic-type-a static-type-b dynamic-type-c)))

; -------------------------------------------------------------------

(define (expressions-expression-option ($expressions : Expressions)) : (Option Expression)
  (option-bind (single (expressions-structure $expressions)) $type
    (expression (expressions-syntax $expressions) $type)))

(define (expressions-symbol-rhs
  ($expressions : Expressions)
  ($symbol : Symbol))
  : (Option Expressions)
  (option-bind (expressions-expression-option $expressions) $expression
    (bind $type (expression-type $expression)
      (and (field? $type) (equal? (field-symbol $type) `the)
        (expressions 
          (expression-syntax $expression)
          (field-structure $type))))))

(define (expressions-rhs-option ($expressions : Expressions)) : (Option Tuple)
  (option-app expression-rhs-tuple-option
    (expressions-expression-option $expressions)))

(check-equal?
  (option-app map expression-sexp-type
    (expressions-rhs-option
      (expressions syntax-a 
        (structure 
          (field `foo
            (structure type-b type-c))))))
  (stack
    (pair `(unsafe-car a) type-b)
    (pair `(unsafe-cdr a) type-c)))

(check-equal?
  (expressions-rhs-option
    (expressions syntax-a 
      (structure 
        (field `foo null)
        (field `bar null))))
  #f)

(check-equal?
  (expressions-rhs-option
    (expressions syntax-a (structure (racket))))
  #f)

; --------------------------------------------------------------

(define (expression-apply-expressions
  ($lhs-expression : Expression)
  ($rhs-expressions : Expressions))
  : (Option Expressions)
  (option-app expressions
    (make-syntax
      `(call-with-values
        (lambda () ,(expressions-syntax $rhs-expressions))
        ,(expression-syntax $lhs-expression)))
    (type-apply-structure 
      (expression-type $lhs-expression)
      (expressions-structure $rhs-expressions))))

(check-equal?
  (option-app expressions-sexp-structure
    (expression-apply-expressions
      (expression #`fn (arrow (structure type-a) (structure type-b)))
      (expressions #`pkg (structure type-a))))
  (pair 
    `(call-with-values (lambda () pkg) fn)
    (structure type-b)))

(check-equal?
  (expression-apply-expressions
    (expression #`fn (arrow (structure type-a) (structure type-b)))
    (expressions #`pkg (structure type-b)))
  #f)

; -------------------------------------------------------------------------

(define (expressions-lift-structure ($expressions : Expressions)) : (Option Structure)
  (structure-lift (expressions-structure $expressions)))

; ---------------------------------------------------------------------

(define (expressions-sexp-option ($expressions : Expressions)) : (Option Sexp)
  (option-app syntax->datum (expressions-syntax-option $expressions)))

(define (tuple-does-expressions
  ($tuple : Tuple)
  ($expressions : Expressions)) : Expressions
  (expressions
    (make-syntax 
      `(lambda 
        ,(reverse (tuple-syntax-stack $tuple))
        ,(expressions-syntax $expressions)))
    (structure 
      (arrow
        (tuple-structure $tuple)
        (expressions-structure $expressions)))))

(check-equal?
  (expressions-sexp
    (tuple-does-expressions
      (tuple 
        (expression #`nul number-type)
        (expression #`txt text-type))
      (expressions 
        #`(string-append (number->string num) txt)
        (structure text-type))))
  (expressions-sexp
    (expressions
      #`(lambda (nul txt) (string-append (number->string num) txt))
      (structure (recipe! number-type text-type (doing text-type))))))

; --------------------------------------------------------------------

(define (tuple-ref ($tuple : Tuple) ($index : Exact-Nonnegative-Integer)) : Expression
  (option-or 
    (stack-ref-default $tuple $index #f)
    (error "top: index out of bounds")))
