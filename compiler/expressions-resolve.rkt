#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/option
  leo/compiler/expression
  leo/compiler/expression-resolve
  leo/compiler/expressions
  leo/compiler/expressions-utils)

(define (expressions-resolve-first-fn 
  ($expressions : Expressions) 
  ($fn : (-> Expression (Option Expressions)))) 
  : (Option Expressions)
  (expressions-resolve-fn $expressions
    (lambda (($tuple : Tuple))
      (tuple-resolve-first-fn $tuple $fn))))
