#lang leo/typed

(require
  leo/compiler/binding
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
  (scope : Scope)
  (lhs-structure : Structure)
  (rhs-structure : Structure)
  (expression-option : (Option Expression)))

(define
  (compile-repeat-expression
    ($scope : Scope)
    ($lhs-structure : Structure)
    ($syntax-list : (Listof Syntax)))
  : Expression
  (repeat-compiler-expression
    (fold
      (repeat-compiler $scope $lhs-structure null-structure #f)
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
  (define $scope
    (repeat-compiler-scope $repeat-compiler))
  (define $lhs-structure
    (repeat-compiler-lhs-structure $repeat-compiler))
  (define $rhs-structure
    (repeat-compiler-rhs-structure $repeat-compiler))
  (define $arrow
    (arrow $lhs-structure $rhs-structure))
  (define $lhs-scope
    (structure-generate-scope $lhs-structure))
  (define $repeat-binding
    (type-generate-binding $arrow))
  (define $doing-scope
    (push (push-stack $scope $lhs-scope) $repeat-binding))
  (define $doing-expressions
    (compile-expressions-recursively
      $doing-scope
      $syntax-list))
  (define $doing-syntax-option
    (expressions-syntax-option $doing-expressions))
  (define $doing-structure
    (expressions-structure $doing-expressions))
  (unless
    (structure-matches? $doing-structure $rhs-structure)
    (error "repeat type mismatch"))
  (define $repeat-identifier-option (binding-identifier-option $repeat-binding))
  (define $syntax
    (and $repeat-identifier-option $doing-syntax-option
      (make-syntax
        `(letrec
          ((
            ,$repeat-identifier-option
            (lambda
              ,(reverse (scope-identifier-stack $lhs-scope))
              ,$doing-syntax-option)))
          ,$repeat-identifier-option))))
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
