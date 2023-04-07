#lang leo/typed

(require
  leo/compiler/compile-recursively
  leo/compiler/type
  leo/compiler/type-match
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/expressions
  leo/compiler/syntax-type
  leo/compiler/syntax-utils
  leo/compiler/ingredients
  leo/compiler/generate-temporary
  leo/compiler/ingredients-utils)

(data repeat-compiler
  (tuple : Tuple)
  (repeated-tuple : Tuple)
  (structure : Structure)
  (expressions-option : (Option Expressions)))

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
  (repeat-compiler-set-expressions
    ($repeat-compiler : Repeat-Compiler)
    ($expressions : Expressions))
  : Repeat-Compiler
  (struct-copy repeat-compiler $repeat-compiler
    (expressions-option $expressions)))

(define
  (repeat-compiler-apply-doing
    ($repeat-compiler : Repeat-Compiler)
    ($syntax-list : (Listof Syntax)))
  : Repeat-Compiler
  (define $tuple
    (repeat-compiler-tuple $repeat-compiler))
  (define $repeated-tuple
    (repeat-compiler-repeated-tuple $repeat-compiler))
  (define $arrow
    (arrow
      (tuple-structure $repeated-tuple)
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
  (define $doing-expressions
    (compile-expressions-recursively
      $doing-tuple
      $syntax-list))
  (define $doing-syntax
    (expressions-syntax $doing-expressions))
  (define $doing-structure
    (expressions-structure $doing-expressions))
  (unless
    (structure-matches?
      $doing-structure
      (repeat-compiler-structure $repeat-compiler))
    (error "repeat type mismatch"))
  (define $syntax
    (cond
      ($tmp-option
        (make-syntax
          `(letrec
            ((
              ,$tmp-option
              (lambda
                ,(reverse (tuple-syntax-stack $repeated-tuple))
                ,$doing-syntax)))
            (,$tmp-option ,@(tuple-syntax-stack $repeated-tuple)))))
      (else $doing-syntax)))
  (define $expressions
    (expressions $syntax $doing-structure))
  (repeat-compiler-set-expressions
    $repeat-compiler
    $expressions))

(define
  (repeat-compiler-plus-syntax
    ($repeat-compiler : Repeat-Compiler)
    ($syntax : Syntax))
  : Repeat-Compiler
  (when (repeat-compiler-expressions-option $repeat-compiler)
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
    (repeat-compiler-expressions-option $repeat-compiler)
    (error "no doing")))
