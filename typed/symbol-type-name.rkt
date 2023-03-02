#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/string
  leo/typed/testing)

(define (symbol-type-name ($symbol : Symbol)) : Symbol
  (string->symbol
    (string-join 
      (map string-titlecase 
        (string-split (symbol->string $symbol) "-"))
      "-")))

(check-equal? (symbol-type-name `foo) `Foo)
(check-equal? (symbol-type-name `big-apple) `Big-Apple)
