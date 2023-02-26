#lang typed/racket/base

(provide (all-defined-out))

(require leo/typed/base)

(data script (line-list : (Listof Line)))

(define-type Line (U Field Native))

(data field (symbol : Symbol) (script : Script))

(data native (any : Any))
