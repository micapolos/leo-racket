#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-sexp
  leo/compiler/expression)

(data switch
  (syntax-stack : (Stackof Syntax))
  (type : Type))

(define null-switch-option : (Option Switch) #f)

(define (switch-sexp ($switch : Switch)) : Sexp
  `(switch
    (syntax-stack ,@(reverse (map syntax->datum (switch-syntax-stack $switch))))
    ,(type-sexp (switch-type $switch))))

(define (expression-switch ($expression : Expression)) : Switch
  (switch 
    (stack (expression-syntax $expression))
    (expression-type $expression)))

(define (switch-plus-expression ($switch : Switch) ($expression : Expression)) : Switch
  (unless 
    (equal? 
      (expression-type $expression)
      (switch-type $switch))
    (error 
      (format 
        "switch expression type mismatch, expected ~a, had ~a"
        (switch-type $switch)
        (expression-type $expression))))
  (switch
    (push 
      (switch-syntax-stack $switch)
      (expression-syntax $expression))
    (switch-type $switch)))

(define (switch-option-plus-expression ($switch-option : (Option Switch)) ($expression : Expression)) : Switch
  (or
    (and $switch-option (switch-plus-expression $switch-option $expression))
    (expression-switch $expression)))
