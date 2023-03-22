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

(define leo-writer? : (Parameter Boolean) (make-parameter #f))

(define (value-displayln ($value : Value))
  (if (leo-writer?)
    (displayln (sexp-string (value-sexp $value)))
    (displayln (value-sexp $value))))
