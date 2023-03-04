#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/type
  leo/compiler/typed)

(data package 
  (syntax : Syntax)
  (structure : Structure))

(define (package-sexp-structure ($package : Package)) : (Pairof Sexp Structure)
  (pair
    (syntax->datum (package-syntax $package))
    (package-structure $package)))
