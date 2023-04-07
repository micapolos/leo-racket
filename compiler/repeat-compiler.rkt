#lang leo/typed

(require
  leo/compiler/compile-recursively
  leo/compiler/type
  leo/compiler/type-match
  leo/compiler/expression
  leo/compiler/expressions
  leo/compiler/syntax-type
  leo/compiler/ingredients
  leo/compiler/generate-temporary
  leo/compiler/ingredients-utils)

(data repeat-compiler
  (tuple : Tuple)
  (repeated-structure : Structure)
  (structure : Structure)
  (doing-ingredients-option : (Option Ingredients)))

(define
  (repeat-compiler-plus-type
    ($repeat-compiler : Repeat-Compiler)
    ($type : Type))
  : Repeat-Compiler
  (struct-copy repeat-compiler $repeat-compiler
    (structure
      (push
        (repeat-compiler-structure $repeat-compiler)
        $type))))

(define
  (repeat-compiler-set-doing-ingredients
    ($repeat-compiler : Repeat-Compiler)
    ($doing-ingredients : Ingredients))
  : Repeat-Compiler
  (struct-copy repeat-compiler $repeat-compiler
    (doing-ingredients-option $doing-ingredients)))

(define
  (repeat-compiler-apply-doing
    ($repeat-compiler : Repeat-Compiler)
    ($syntax-list : (Listof Syntax)))
  : Repeat-Compiler
  (define $tuple (repeat-compiler-tuple $repeat-compiler))
  (define $arrow
    (arrow
      (repeat-compiler-repeated-structure $repeat-compiler)
      (repeat-compiler-structure $repeat-compiler)))
  (define $tmp-option
    (type-generate-temporary-option $arrow))
  (define $doing-tuple
    (cond
      ($tmp-option
        (push
          (repeat-compiler-tuple $repeat-compiler)
          (expression $tmp-option $arrow)))
      (else $tuple)))
  (repeat-compiler-set-doing-ingredients
    $repeat-compiler
    (compile-ingredients-recursively
      $doing-tuple
      $syntax-list)))

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
  (repeat-compiler-apply-syntax
    ($repeat-compiler : Repeat-Compiler)
    ($syntax : Syntax))
  : Repeat-Compiler
  (repeat-compiler-plus-type
    $repeat-compiler
    (syntax-type $syntax)))

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
