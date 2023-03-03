#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/testing
  leo/typed/stack
  leo/typed/base
  leo/typed/option
  leo/compiler/binding
  leo/compiler/expression
  leo/compiler/syntax-expression
  leo/compiler/type
  leo/compiler/racket
  leo/compiler/body)

(data compiler
  (binding-stack : (Stackof Binding))
  (body : Body))

(define null-compiler (compiler null null-body))

; -----------------------------------------------------------------

(define (compiler-append-syntax-list 
  ($compiler : Compiler)
  ($syntax-list : (Listof Syntax)))
  : Compiler
  (fold $compiler $syntax-list compiler-append-syntax))

(define (compiler-append-syntax 
  ($compiler : Compiler) 
  ($syntax : Syntax))
  : Compiler
  (or
    (option-bind 
      (syntax-expression-option $syntax)
      $expression 
      (compiler-append-expression $compiler $expression))
    (error "TODO")))

(define (compiler-append-expression 
  ($compiler : Compiler) 
  ($expression : Expression))
  : Compiler
  (compiler
    (compiler-binding-stack $compiler)
    (body-append-expression (compiler-body $compiler) $expression)))

(check-equal?
  (map syntax->datum
    (body-values-syntax-stack
      (compiler-body
        (compiler-append-syntax-list 
          null-compiler 
          (list #`1 #`"foo")))))
  (stack `(values 1 "foo")))

