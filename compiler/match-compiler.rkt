#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-sexp
  leo/compiler/binding
  leo/compiler/scope
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/generate-temporary
  leo/compiler/ingredients-utils
  leo/compiler/expressions-utils
  leo/compiler/compile-ingredients)

(data match-compiler
  (scope : Scope)
  (cases-tuple : Tuple)
  (remaining-type-list : (Listof Type)))

(define null-match-compiler (match-compiler null-scope null-tuple null))

(define (match-compiler-sexp ($match-compiler : Match-Compiler)) : Sexp
  `(match-compiler
    ,(scope-sexp (match-compiler-scope $match-compiler))
    (cases ,(tuple-sexp (match-compiler-cases-tuple $match-compiler)))
    (remaining ,(map type-sexp (match-compiler-remaining-type-list $match-compiler)))))

(define (match-compiler-plus-syntax 
  ($match-compiler : Match-Compiler)
  ($syntax : Syntax))
  : Match-Compiler
  (define $scope (match-compiler-scope $match-compiler))
  (define $cases-tuple (match-compiler-cases-tuple $match-compiler))
  (define $remaining-type-list (match-compiler-remaining-type-list $match-compiler))
  (when (null? $remaining-type-list) (error "no more remaining choices"))
  (define $type (car $remaining-type-list))
  (define $tmp (type-generate-temporary-option $type))
  (define $binding (binding $type $tmp))
  (define $new-scope (push $scope $binding))
  (define $ingredients (compile-ingredients $new-scope (list $syntax)))
  (define $expressions (ingredients-expressions $ingredients))
  (define $expression (expressions-expression-option $expressions))
  (unless $expression (error "match expected expression"))
  (match-compiler 
    $scope 
    (push $cases-tuple $expression)
    (cdr $remaining-type-list)))

(check-equal?
  (match-compiler-sexp
    (match-compiler-plus-syntax
      (match-compiler 
        null-scope 
        null-tuple
        (list type-a type-b))
      syntax-c))
  `(match-compiler
    (scope ())
    (cases (tuple (expression recurse racket)))
    (remaining ((b racket)))))
