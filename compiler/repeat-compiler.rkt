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
  (lhs-structure : Structure)
  (rhs-structure : Structure)
  (expression-option : (Option Expression)))

(define
  (compile-repeat-expression
    ($tuple : Tuple)
    ($lhs-structure : Structure)
    ($syntax-list : (Listof Syntax)))
  : Expression
  (repeat-compiler-expression
    (fold
      (repeat-compiler $tuple $lhs-structure null-structure #f)
      $syntax-list
      repeat-compiler-plus-syntax)))

(define
  (repeat-compiler-plus-type
    ($repeat-compiler : Repeat-Compiler)
    ($type : Type))
  : Repeat-Compiler
  (struct-copy repeat-compiler $repeat-compiler
    (rhs-structure
      (push
        (repeat-compiler-rhs-structure $repeat-compiler)
        $type))))

(define
  (repeat-compiler-set-expression
    ($repeat-compiler : Repeat-Compiler)
    ($expression : Expression))
  : Repeat-Compiler
  (struct-copy repeat-compiler $repeat-compiler
    (expression-option $expression)))

(define
  (repeat-compiler-apply-doing
    ($repeat-compiler : Repeat-Compiler)
    ($syntax-list : (Listof Syntax)))
  : Repeat-Compiler
  (define $tuple
    (repeat-compiler-tuple $repeat-compiler))
  (define $lhs-structure
    (repeat-compiler-lhs-structure $repeat-compiler))
  (define $rhs-structure
    (repeat-compiler-rhs-structure $repeat-compiler))
  (define $arrow
    (arrow $lhs-structure $rhs-structure))
  (define $lhs-tuple
    (structure-generate-tuple $lhs-structure))
  (define $repeat-expression
    (type-generate-expression $arrow))
  (define $doing-tuple
    (push (push-stack $tuple $lhs-tuple) $repeat-expression))
  (define $doing-expressions
    (compile-expressions-recursively
      $doing-tuple
      $syntax-list))
  (define $doing-syntax
    (expressions-syntax $doing-expressions))
  (define $doing-structure
    (expressions-structure $doing-expressions))
  (unless
    (structure-matches? $doing-structure $rhs-structure)
    (error "repeat type mismatch"))
  (define $syntax
    (make-syntax
      `(letrec
        ((
          ,(expression-syntax $repeat-expression)
          (lambda
            ,(reverse (tuple-syntax-stack $lhs-tuple))
            ,$doing-syntax)))
        ,(expression-syntax $repeat-expression))))
  (define $expression
    (expression $syntax $arrow))
  (repeat-compiler-set-expression
    $repeat-compiler
    $expression))

(define
  (repeat-compiler-plus-syntax
    ($repeat-compiler : Repeat-Compiler)
    ($syntax : Syntax))
  : Repeat-Compiler
  (when (repeat-compiler-expression-option $repeat-compiler)
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
  (repeat-compiler-expression
    ($repeat-compiler : Repeat-Compiler))
  : Expression
  (option-or
    (repeat-compiler-expression-option $repeat-compiler)
    (error "no doing")))
