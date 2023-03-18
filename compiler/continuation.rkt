#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/option
  leo/compiler/type
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/syntax-utils)

(data ingredients
  (structure : Structure)
  (syntax-fn : (-> Syntax)))

(define null-ingredients 
  (ingredients 
    null-structure 
    (lambda () null-syntax)))

(define (ingredients-syntax ($ingredients : Ingredients)) : Syntax
  (#%app (ingredients-syntax-fn $ingredients)))

(define (ingredients-expressions ($ingredients : Ingredients)) : Expressions
  (expressions 
    (ingredients-syntax $ingredients) 
    (ingredients-structure $ingredients)))

; (define (ingredients-append ($lhs : Ingredients) ($rhs : Ingredients)) : Ingredients
;   (define $lhs-structure (ingredients-structure $lhs))
;   (define $rhs-structure (ingredients-structure $rhs))
;   (ingredients
;     (push-stack
;       (ingredients-structure $lhs)
;       (ingredients-structure $rhs))
;     (lambda ()
;       (define $lhs-syntax (ingredients-syntax $lhs))
;       (define $rhs-syntax (ingredients-syntax $rhs))
;       (define $lhs-binder (expretmp-stack ())

