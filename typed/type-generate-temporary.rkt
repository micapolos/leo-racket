#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/type-symbol
  leo/testing)

(define (type-generate-temporary ($type : Type)) : Identifier
  (define $symbol (type-symbol $type))
  (car (generate-temporaries (list (or $symbol `tmp)))))
