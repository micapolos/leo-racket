#lang racket/base

(provide (all-defined-out))

(require
  leo/testing
  (for-syntax racket/base))

; -----------------------------------------------------------------------------------------------

(define-syntax (local $syntax)
  (syntax-case $syntax ()
    ((_ $module $identifier)
      #`(let () (local-require (only-in $module $identifier)) $identifier))))

(check-equal? (local leo/testing check) check)

; ------------------------------------------------------------------------------------------------

(define-syntax (local-app $syntax)
  (syntax-case $syntax ()
    ((_ $module $identifier $body ...)
      #`(#%app (local $module $identifier) $body ...))))

(check-equal? (local-app racket/unsafe/ops unsafe-fx- 3 2) 1)

(define (symbol-append . $symbols)
  (string->symbol (apply string-append (map symbol->string $symbols))))

(check-equal?
  (symbol-append `foo `- `bar)
  `foo-bar)
