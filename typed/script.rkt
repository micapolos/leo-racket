#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/base)

(define-type Line (U Field Native))
(define-type Script (Listof Line))

(data field (symbol : Symbol) (script : Script))
(data native (any : Any))
