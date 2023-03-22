#lang typed/racket/base

(provide 
  (all-defined-out)
  (all-from-out
    racket/function
    leo/typed/stack
    leo/compiler/type))

(require
  racket/function
  leo/typed/stack
  leo/compiler/type
  leo/compiler/type-sexp
  leo/compiler/sexp-string)

(define (value-displayln ($format : (U 'racket 'leo)) ($value : Value))
  (if (equal? $format 'leo)
    (displayln (sexp-string (value-sexp $value)))
    (displayln (value-sexp $value))))
