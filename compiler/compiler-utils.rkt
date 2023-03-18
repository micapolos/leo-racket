#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/compiler
  leo/compiler/ingredients
  leo/compiler/ingredients-utils
  leo/compiler/compiler-plus-expressions
  leo/compiler/compile-ingredients
  leo/compiler/sexp-expression)

(define (compiler-with-ingredients ($compiler : Compiler) ($ingredients : Ingredients)) : Compiler
  (struct-copy compiler $compiler (ingredients $ingredients)))

(define (compiler-apply-racket ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler 
  (compiler-with-ingredients $compiler
    (ingredients-apply-racket 
      (ingredients-plus 
        (compiler-ingredients $compiler)
        (compile-ingredients
          (compiler-tuple $compiler)
          $syntax-list)))))

(define (compiler-apply-type ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler 
  (compiler-with-ingredients $compiler
    (ingredients-apply-type
      (ingredients-plus 
        (compiler-ingredients $compiler)
        (compile-ingredients
          (compiler-tuple $compiler)
          $syntax-list)))))

(define (compiler-apply-quote ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-plus-quoted-tuple $compiler
    (sexp-list-tuple
      (map syntax->datum $syntax-list))))

(define (compiler-apply-apply ($compiler : Compiler) ($syntax-list : (Listof Syntax))) : Compiler
  (compiler-with-ingredients $compiler
    (tuple-apply-ingredients
      (compiler-tuple $compiler)
      (ingredients-plus 
        (compiler-ingredients $compiler)
        (compile-ingredients
          (compiler-tuple $compiler)
          $syntax-list)))))
      
