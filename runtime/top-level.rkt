#lang typed/racket/base

(provide 
  (all-defined-out)
  (all-from-out
    leo/typed/stack
    leo/compiler/type))

(require
  leo/typed/stack
  leo/compiler/type
  leo/compiler/type-sexp
  leo/compiler/sexp-string)

(define (value-displayln ($value : Value))
  (displayln (sexp-string (value-sexp $value))))
