 #lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/racket)

(define-type Line (U Field Racket))

(data field (symbol : Symbol) (line-list : (Listof Line)))
