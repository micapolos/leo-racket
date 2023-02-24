#lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/type
  leo/typed/type-symbol
  leo/typed/testing)

(define (type-generate-temporary ($type : Type)) : Identifier
  (define $symbol (type-symbol $type))
  (car (generate-temporaries (list (or $symbol `tmp)))))

(check-equal? (identifier? (type-generate-temporary `(foo 1 2))) #t)
(check-equal? (identifier? (type-generate-temporary 123)) #t)
