#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/compiler/expressions
  leo/compiler/expression)

(data package
  (expressions-option : (Option Expressions))
  (tuple : Tuple))

(define null-package (package #f null-tuple))

(define (expressions-package ($expressions : Expressions))
  (package $expressions null-tuple))

(define (tuple-package ($tuple : Tuple))
  (package #f $tuple))
