#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/expression-compiler
  leo/compiler/generate-temporary)

(define (expression-compiler-append-expression
  ($expression-compiler : Expression-Compiler)
  ($expression : Expression))
  : Expression-Compiler
  (define $type (expression-type $expression))
  (define $temporary (type-generate-temporary $type))
  (expression-compiler
    (push 
      (expression-compiler-bound-expression-stack $expression-compiler)
      (expression $temporary $type))
    (push
      (expression-compiler-compiled-expression-stack $expression-compiler)
      $expression)))
