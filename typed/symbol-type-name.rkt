#lang typed/racket/base

(provide (all-defined-out))

(require 
  racket/string
  leo/typed/testing)

(define (symbol-type-name ($symbol : Symbol) ($generic? : Boolean #f)) : Symbol
  (string->symbol
    (string-append
      (string-join 
        (map string-titlecase 
          (string-split (symbol->string $symbol) "-"))
        "-")
      (if $generic? "of" ""))))

(check-equal? (symbol-type-name `foo) `Foo)
(check-equal? (symbol-type-name `big-apple) `Big-Apple)

(check-equal? (symbol-type-name `foo #t) `Fooof)
(check-equal? (symbol-type-name `big-apple #t) `Big-Appleof)
