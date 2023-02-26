#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/base)

(define-type Line (U Field Native))

(data field (symbol : Symbol) (line-list : (Listof Line)))

(data native (any : Any))
