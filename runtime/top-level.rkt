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
  leo/compiler/value
  leo/compiler/value-sexp
  leo/compiler/type
  leo/compiler/type-sexp
  leo/compiler/sexp-string
  leo/compiler/value-utils)

(define (value-displayln ($format : (U 'racket 'leo)) ($value : Value))
  (if (equal? $format 'leo)
    (displayln (sexp-string (value-sexp $value)))
    (displayln (value-sexp $value))))

(define (packet-displayln ($format : (U 'racket 'leo)) ($packet : Packet))
  (for-each
    (curry value-displayln $format)
    (reverse $packet)))

(define (any-stack-structure-displayln ($format : (U 'racket 'leo)) ($any-stack : (Stackof Any)) ($structure : Structure))
  (packet-displayln $format
    (any-stack-structure-packet $any-stack $structure)))
