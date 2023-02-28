 #lang typed/racket/base

(provide (all-defined-out))

(require 
  leo/typed/base
  leo/typed/stack
  leo/compiler/sourced
  leo/compiler/racket)

(define-type Line (U Phrase Racket))

(define-type Script (Stackof (Sourced Line)))

(struct phrase (
  (sourced-symbol : (Sourced Symbol)) 
  (line-stack : (Stackof Line)))
  #:transparent
  #:type-name Phrase)
