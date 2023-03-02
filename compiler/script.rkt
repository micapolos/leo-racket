 #lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/compiler/sourced
  leo/compiler/racket)

(define-type Line (U Phrase Racket))

(define-type Script (Stackof Line))

(data phrase (symbol : Symbol) (script : Script))
