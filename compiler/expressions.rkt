#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/type
  leo/compiler/typed)

(data expressions 
  (syntax : Syntax)
  (structure : Structure))

(define (expressions-sexp-structure ($expressions : Expressions)) : (Pairof Sexp Structure)
  (pair
    (syntax->datum (expressions-syntax $expressions))
    (expressions-structure $expressions)))
