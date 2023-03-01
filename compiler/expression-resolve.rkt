#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/type
  leo/compiler/binding
  leo/compiler/binding-resolve
  leo/compiler/expression)

; TODO: unify Binding and Expression
(define (expression-binding ($expression : Expression)) : Binding
  (binding 
    (expression-type $expression)
    (expression-syntax $expression)))

(define (expression-stack-resolve 
  ($expression-stack : (Stackof Expression)))
  : (Option Expression)
  #f)

(define (expression-stack-resolve-get
  ($expression-stack : (Stackof Expression)))
  : (Option Expression)
  (and
    (= (length $expression-stack) 2)
    (let ()
      (define $lhs-expression (pop-top $expression-stack))
      (define $rhs-expression (top $expression-stack))
      (define $lhs-type (expression-type $lhs-expression))
      (and
        (field? $lhs-type)
        #f))))
