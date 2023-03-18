#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/typed
  leo/compiler/syntax-utils)

(data expressions 
  (syntax : Syntax)
  (structure : Structure))

(define null-expressions (expressions null-syntax null-structure))

; TODO: Refactor expression-syntax
(define (expressions-syntax-option ($expressions : Expressions)) : (Option Syntax)
  (and 
    (structure-dynamic? (expressions-structure $expressions)) 
    (expressions-syntax $expressions)))

(define (expressions-sexp-structure ($expressions : Expressions)) : (Pairof Sexp Structure)
  (pair
    (syntax->datum (expressions-syntax $expressions))
    (expressions-structure $expressions)))
