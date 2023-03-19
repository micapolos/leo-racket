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
  (tuple : Tuple)
  (identifier-option : (Option Identifier))
  (switch-option : (Option Switch))
  (remaining-type-list : (Listof Type)))

(define null-match-compiler (match-compiler null-tuple #f #f null))

(define (match-compiler-switch ($match-compiler : Match-Compiler)) : Switch
  (unless (null? (match-compiler-remaining-type-list $match-compiler))
    (error "not all cases handled"))
  (option-or (match-compiler-switch-option $match-compiler)
    (error "empty switch")))

(define (match-compiler-sexp ($match-compiler : Match-Compiler)) : Sexp
  `(match-compiler
    ,(tuple-sexp (match-compiler-tuple $match-compiler))
    ,(option-app switch-sexp (match-compiler-switch-option $match-compiler))
    (remaining ,(map type-sexp (match-compiler-remaining-type-list $match-compiler)))))

(define (match-compiler-plus-syntax 
  ($match-compiler : Match-Compiler)
  ($syntax : Syntax))
  : Match-Compiler
  (define $tuple (match-compiler-tuple $match-compiler))
  (define $identifier-option (match-compiler-identifier-option $match-compiler))
  (define $switch-option (match-compiler-switch-option $match-compiler))
  (define $remaining-type-list (match-compiler-remaining-type-list $match-compiler))
  (when (null? $remaining-type-list) (error "no more remaining choices"))
  (define $type (car $remaining-type-list))
  (define $new-expression (expression (or $identifier-option null-syntax) $type))
  (define $new-tuple (push $tuple $new-expression))
  (define $ingredients (compile-ingredients $new-tuple (list $syntax)))
  (define $expressions (ingredients-expressions $ingredients))
  (define $expression (expressions-expression-option $expressions))
  (unless $expression (error "match expected expression"))
  (match-compiler 
    $tuple
    $identifier-option
    (switch-option-plus-expression $switch-option $expression)
    (cdr $remaining-type-list)))

(parameterize 
  ((compile-ingredients-parameter 
    (lambda (($tuple : Tuple) ($syntax-list : (Listof Syntax)))
      (ingredients
        (expression-expressions
          (expression #`switched (field! `switched)))))))

  (check-equal?
    (match-compiler-sexp
      (match-compiler-plus-syntax
        (match-compiler 
          null-tuple
          #`value
          null-switch-option
          (list (field! `foo) (field! `bar)))
        (make-syntax `case)))
    `(match-compiler
      (tuple)
      (switch (syntax-stack (values)) switched)
      (remaining (bar))))

  (check-equal?
    (match-compiler-sexp
      (match-compiler-plus-syntax
        (match-compiler 
          null-tuple 
          #`value
          (switch
            (stack #`zero #`one)
            (field! `switched))
          (list (field! `two) (field! `three)))
        (make-syntax `switched)))
    `(match-compiler
      (tuple)
      (switch (syntax-stack zero one (values)) switched)
      (remaining (three)))))