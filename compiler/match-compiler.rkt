#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/typed/option
  leo/typed/testing
  leo/compiler/type
  leo/compiler/type-utils
  leo/compiler/type-sexp
  leo/compiler/ingredients
  leo/compiler/binding
  leo/compiler/scope
  leo/compiler/switch
  leo/compiler/syntax-utils
  leo/compiler/expression
  leo/compiler/expression-utils
  leo/compiler/generate-temporary
  leo/compiler/ingredients-utils
  leo/compiler/expressions-utils
  leo/compiler/compile-ingredients)

(data cases
  (syntax-stack : (Stackof Syntax))
  (type : Type))

(data match-compiler
  (scope : Scope)
  (identifier-option : (Option Identifier))
  (switch-option : (Option Switch))
  (remaining-type-list : (Listof Type)))

(define null-match-compiler (match-compiler null-scope #f #f null))

(define (match-compiler-switch ($match-compiler : Match-Compiler)) : Switch
  (unless (null? (match-compiler-remaining-type-list $match-compiler))
    (error "not all cases handled"))
  (option-or (match-compiler-switch-option $match-compiler)
    (error "empty switch")))

(define (match-compiler-sexp ($match-compiler : Match-Compiler)) : Sexp
  `(match-compiler
    ,(scope-sexp (match-compiler-scope $match-compiler))
    ,(option-app switch-sexp (match-compiler-switch-option $match-compiler))
    (remaining ,(map type-sexp (match-compiler-remaining-type-list $match-compiler)))))

(define (match-compiler-plus-syntax 
  ($match-compiler : Match-Compiler)
  ($syntax : Syntax))
  : Match-Compiler
  (define $scope (match-compiler-scope $match-compiler))
  (define $identifier-option (match-compiler-identifier-option $match-compiler))
  (define $switch-option (match-compiler-switch-option $match-compiler))
  (define $remaining-type-list (match-compiler-remaining-type-list $match-compiler))
  (when (null? $remaining-type-list) (error "no more remaining choices"))
  (define $type (car $remaining-type-list))
  (define $binding (binding $type $identifier-option))
  (define $new-scope (push $scope $binding))
  (define $ingredients (compile-ingredients $new-scope (list $syntax)))
  (define $expressions (ingredients-expressions $ingredients))
  (define $expression (expressions-expression-option $expressions))
  (unless $expression (error "match expected expression"))
  (match-compiler 
    $scope
    $identifier-option
    (switch-option-plus-expression $switch-option $expression)
    (cdr $remaining-type-list)))

(parameterize 
  ((compile-ingredients-parameter 
    (lambda (($scope : Scope) ($syntax-list : (Listof Syntax)))
      (ingredients
        (expression-expressions
          (expression #`switched (field! `switched)))))))

  (check-equal?
    (match-compiler-sexp
      (match-compiler-plus-syntax
        (match-compiler 
          null-scope
          #`value
          null-switch-option
          (list (field! `foo) (field! `bar)))
        (make-syntax `case)))
    `(match-compiler
      (scope)
      (switch (syntax-stack #f) switched)
      (remaining (bar))))

  (check-equal?
    (match-compiler-sexp
      (match-compiler-plus-syntax
        (match-compiler 
          null-scope 
          #`value
          (switch
            (stack #`zero #`one)
            (field! `switched))
          (list (field! `two) (field! `three)))
        (make-syntax `switched)))
    `(match-compiler
      (scope)
      (switch (syntax-stack zero one #f) switched)
      (remaining (three)))))
