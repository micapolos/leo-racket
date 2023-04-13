#lang leo/typed

(require 
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/syntax-utils)

(data expressions 
  (syntax-option : (Option Syntax))
  (structure : Structure))

(define (expressions-sexp-structure ($expressions : Expressions)) : (Pairof Sexp Structure)
  (pair
    (option-app syntax->datum (expressions-syntax-option $expressions))
    (expressions-structure $expressions)))
