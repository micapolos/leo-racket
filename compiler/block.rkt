#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/typed/base
  leo/compiler/type
  leo/compiler/type-sexp)

(data block 
  (syntax : Syntax)
  (arrow : Arrow))

(define (block-sexp ($block : Block)) : Sexp
  `(block
    ,(syntax->datum (block-syntax $block))
    ,(type-sexp (block-arrow $block))))