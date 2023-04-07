#lang leo/typed

(require
  leo/compiler/compile-recursively
  leo/compiler/type
  leo/compiler/type-match
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/syntax-type
  leo/compiler/ingredients
  leo/compiler/ingredients-utils)

(data repeat-compiler
  (tuple : Tuple)
  (structure : Structure)
  (doing-ingredients-option : (Option Ingredients)))

(define
  (repeat-compiler-plus-syntax
    ($repeat-compiler : Repeat-Compiler)
    ($syntax : Syntax))
  : Repeat-Compiler
  (when (repeat-compiler-doing-ingredients-option $repeat-compiler)
    (error "already has doing"))
  (or
    (syntax-match-symbol-args $syntax $symbol $args
      (case $symbol
        ((doing) (repeat-compiler-apply-doing $repeat-compiler $args))
        (else #f)))
    (repeat-compiler-apply-syntax $repeat-compiler $syntax)))

(define
  (repeat-compiler-apply-doing
    ($repeat-compiler : Repeat-Compiler)
    ($syntax-list : (Listof Syntax)))
  : Repeat-Compiler
  ; TODO: Bind repeat structure
  (struct-copy repeat-compiler $repeat-compiler
    (doing-ingredients-option
      (compile-ingredients-recursively
        (repeat-compiler-tuple $repeat-compiler)
        $syntax-list))))

(define
  (repeat-compiler-apply-syntax
    ($repeat-compiler : Repeat-Compiler)
    ($syntax : Syntax))
  : Repeat-Compiler
  (struct-copy repeat-compiler $repeat-compiler
    (structure
      (push
        (repeat-compiler-structure $repeat-compiler)
        (syntax-type $syntax)))))

(define
  (repeat-compiler-expressions
    ($repeat-compiler : Repeat-Compiler))
  : Expressions
  (option-or
    (option-bind (repeat-compiler-doing-ingredients-option $repeat-compiler) $ingredients
      (unless
        (structure-matches?
          (ingredients-structure $ingredients)
          (repeat-compiler-structure $repeat-compiler))
        (error "repeat type mismatch"))
      (ingredients-expressions $ingredients))
    (error "no doing")))
