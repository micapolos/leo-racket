#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/typed/option
  leo/typed/testing
  leo/compiler/block
  leo/compiler/expressions
  leo/compiler/expressions-utils
  leo/compiler/expressions-sexp
  leo/compiler/scope
  leo/compiler/type
  leo/compiler/type-match
  leo/compiler/type-utils)

(define (block-resolve-do ($block : Block) ($expressions : Expressions)) : (Option Expressions)
  (let* (($block-arrow (block-arrow $block))
         ($block-syntax (block-syntax $block))
         ($block-arrow-from-structure (arrow-from-structure $block-arrow))
         ($block-arrow-to-structure (arrow-to-structure $block-arrow))
         ($expressions-structure (expressions-structure $expressions))
         ($expressions-syntax (expressions-syntax $expressions)))
    (and
      (structure-matches? $expressions-structure $block-arrow-from-structure)
      (expressions-do $expressions 
        (lambda (($scope : Scope)) 
          (expressions
            $block-syntax
            $block-arrow-to-structure))))))

(check-equal?
  (option-app expressions-sexp
    (block-resolve-do
      (block #`block
        (arrow 
          (structure dynamic-type-a dynamic-type-b) 
          (structure dynamic-type-c dynamic-type-d)))
      (expressions #`expressions
        (structure dynamic-type-a dynamic-type-b))))
  `(expressions
    (let-values (((tmp-a tmp-b) expressions)) block)
    (structure (c racket) (d racket))))

(check-not
  (option-app expressions-sexp
    (block-resolve-do
      (block #`block
        (arrow 
          (structure dynamic-type-a dynamic-type-b) 
          (structure dynamic-type-c dynamic-type-d)))
      (expressions #`expressions
        (structure dynamic-type-c dynamic-type-d)))))
