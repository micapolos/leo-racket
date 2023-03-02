#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/stack
  leo/compiler/expression
  leo/compiler/field-expression
  leo/compiler/generate-temporary)

(struct expression-compiler (
  (bound-expression-stack : (Stackof Expression))
  (compiled-expression-stack : (Stackof Expression)))
  #:transparent
  #:type-name Expression-Compiler)

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

(define (expression-stack-compile-syntax-list
  ($expression-stack : (Stackof Expression))
  ($syntax-list : (Listof Syntax)))
  : (Stackof Expression)
  (error "TODO"))

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
