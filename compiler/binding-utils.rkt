#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/compiler/binding
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/typed
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/generate-temporary)

(define binding-ab (binding (arrow (structure type-a) (structure type-b)) #`ab))
(define binding-cd (binding (arrow (structure type-c) (structure type-d)) #`cd))

(define (expression-binding ($expression : Expression)) : Binding
  (define $syntax (expression-syntax $expression))
  (binding
    (expression-type $expression)
    (and (identifier? $syntax) $syntax)))

(define (binding-expression ($binding : Binding)) : Expression
  (define $identifier-option (binding-identifier-option $binding))
  (expression
    (or $identifier-option null-syntax)
    (binding-type $binding)))

(define (expression-sexp-type-2 ($expression : Expression)) : (Pairof Sexp Type)
  (pair
    (syntax->datum (expression-syntax $expression))
    (expression-type $expression)))

(check-equal?
  (expression-sexp-type-2 (binding-expression (binding type-a #`b)))
  (pair `b type-a))

(define (type-generate-binding ($type : Type)) : Binding
  (binding $type (type-generate-temporary-option $type)))

(define (unary-binding 
  ($lhs-type : Type)
  ($symbol : Symbol)
  ($return-type : Type)
  ($identifier : Identifier))
  (binding
    (arrow
      (structure 
        $lhs-type
        (field! $symbol))
      (structure $return-type))
    $identifier))

(define (unary-binding-2
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($return-type : Type)
  ($identifier : Identifier))
  (binding
    (arrow
      (structure 
        $lhs-type
        (field! $symbol-1)
        (field! $symbol-2))
      (structure $return-type))
    $identifier))

(define (unary-nested-binding-2
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($return-type : Type)
  ($identifier : Identifier))
  (binding
    (arrow
      (structure 
        $lhs-type
        (field $symbol-1
          (structure
            (field! $symbol-2))))
      (structure $return-type))
    $identifier))

(define (unary-nested-binding-3
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($symbol-3 : Symbol)
  ($return-type : Type)
  ($identifier : Identifier))
  (binding
    (arrow
      (structure 
        $lhs-type
        (field $symbol-1
          (structure 
            (field $symbol-2
              (structure
                (field! $symbol-3))))))
      (structure $return-type))
    $identifier))

(define (binary-binding 
  ($lhs-type : Type)
  ($symbol : Symbol)
  ($rhs-type : Type)
  ($return-type : Type)
  ($identifier : Identifier))
  (binding
    (arrow
      (structure 
        $lhs-type
        (field $symbol (structure $rhs-type)))
      (structure $return-type))
    $identifier))

(define (binary-binding-2
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($rhs-type : Type)
  ($return-type : Type)
  ($identifier : Identifier))
  (binding
    (arrow
      (structure 
        $lhs-type
        (field $symbol-1
          (structure 
            (field $symbol-2 
              (structure $rhs-type)))))
      (structure $return-type))
    $identifier))

(define (binary-binding-3
  ($lhs-type : Type)
  ($symbol-1 : Symbol)
  ($symbol-2 : Symbol)
  ($symbol-3 : Symbol)
  ($rhs-type : Type)
  ($return-type : Type)
  ($identifier : Identifier))
  (binding
    (arrow
      (structure 
        $lhs-type
        (field $symbol-1
          (structure 
            (field $symbol-2 
              (structure 
                (field $symbol-3
                  (structure $rhs-type)))))))
      (structure $return-type))
    $identifier))
