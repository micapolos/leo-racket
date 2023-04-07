#lang leo/typed

(require
  leo/compiler/type
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/ingredients)

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
        (else (repeat-compiler-apply-symbol $repeat-compiler $symbol $args))))
    (repeat-compiler-apply-syntax $repeat-compiler $syntax)))

(define
  (repeat-compiler-apply-doing
    ($repeat-compiler : Repeat-Compiler)
    ($syntax-list : (Listof Syntax)))
  : Repeat-Compiler
  (error "TODO"))

(define
  (repeat-compiler-apply-symbol
    ($repeat-compiler : Repeat-Compiler)
    ($symbol : Symbol)
    ($args : (Listof Syntax)))
  : Repeat-Compiler
  (error "TODO"))

(define
  (repeat-compiler-apply-syntax
    ($repeat-compiler : Repeat-Compiler)
    ($syntax : Syntax))
  : Repeat-Compiler
  (error "TODO"))

(define
  (repeat-compiler-expressions
    ($repeat-compiler : Repeat-Compiler))
  : Expressions
  (error "TODO"))
