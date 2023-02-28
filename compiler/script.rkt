 #lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/compiler/racket)

(define-type Line (U Phrase Racket))

(struct phrase ((symbol : Symbol) (line-stack : (Stackof Line)))
  #:transparent
  #:type-name Phrase)
