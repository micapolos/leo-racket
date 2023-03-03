#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/compiler/package
  leo/compiler/expression)

(define (package-append-expression 
  ($package : Package) 
  ($expression : Expression)) 
  : Package
  (error "TODO"))
