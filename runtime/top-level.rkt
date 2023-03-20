#lang typed/racket/base

(provide (all-defined-out))

(require
  leo/compiler/type
  leo/compiler/type-sexp
  leo/compiler/sexp-string)

(define (displayln-any-list-type-list
  ($any-list : (Listof Any))
  ($type-list : (Listof Type)))
  (for-each
    (lambda (($any : Any) ($type : Type))
      (displayln (sexp-string (value-sexp (value $any $type)))))
    $any-list
    $type-list))
