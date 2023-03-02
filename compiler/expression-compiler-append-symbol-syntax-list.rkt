#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/expression-compiler
  leo/compiler/expression-compiler-append-expression
  leo/compiler/expression-stack-compile-syntax-list
  leo/compiler/field-expression
  leo/compiler/generate-temporary)

(define (expression-compiler-append-symbol-syntax-list
  ($expression-compiler : Expression-Compiler)
  ($symbol : Symbol)
  ($syntax-list : (Listof Syntax)))
  : Expression-Compiler
  (case $symbol
    ((do) (error "do"))
    ((take) (error "take"))
    (else 
      (expression-compiler-append-expression
        $expression-compiler
        (field-expression
          $symbol 
          (expression-stack-compile-syntax-list 
            (expression-compiler-bound-expression-stack $expression-compiler)
            $syntax-list))))))
